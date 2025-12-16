#include <Cocoa/Cocoa.h>
#include <Carbon/Carbon.h>
#include "emacs-module.h"
#include <pthread.h>

int plugin_is_GPL_compatible;

// Global variables to manage state
static id eventMonitor = nil;
static pthread_mutex_t queueMutex;
static NSMutableArray *eventQueue;

// --- Helper: Add event to queue ---
void enqueue_event_data(long keyCode, unsigned long modifierFlags) {
    pthread_mutex_lock(&queueMutex);
    if (!eventQueue) {
        eventQueue = [[NSMutableArray alloc] init];
    }
    
    NSDictionary *eventData = @{
        @"keyCode": @(keyCode),
        @"modifiers": @(modifierFlags)
    };
    [eventQueue addObject:eventData];
    pthread_mutex_unlock(&queueMutex);
}

// --- Module Function: Start Monitor ---
static emacs_value Fime_hook_start(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data) {
    if (eventMonitor) return env->intern(env, "nil"); // Already running

    // Initialize mutex and queue
    pthread_mutex_init(&queueMutex, NULL);
    eventQueue = [[NSMutableArray alloc] init];

    // Add Local Monitor
    // NSEventMaskKeyDown | NSEventMaskFlagsChanged
    NSEventMask mask = NSEventMaskKeyDown | NSEventMaskFlagsChanged; 
    
    eventMonitor = [NSEvent addLocalMonitorForEventsMatchingMask:mask handler:^NSEvent * _Nullable(NSEvent *event) {
        // THIS BLOCK RUNS IN THE RUNLOOP. NO VALID emacs_env HERE!
        
        // 1. Capture data
        long keyCode = [event keyCode];
        unsigned long flags = [event modifierFlags];
        
        // 2. Enqueue for Lisp to pick up later
        enqueue_event_data(keyCode, flags);
        
        // 3. Return event (return nil to consume/block it, return event to pass it on)
        return event; 
    }];

    return env->intern(env, "t");
}

// --- Module Function: Stop Monitor ---
static emacs_value Fime_hook_stop(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data) {
    if (eventMonitor) {
        [NSEvent removeMonitor:eventMonitor];
        eventMonitor = nil;
        eventQueue = nil; // ARC handles release in ObjC ARC mode, or release manually
    }
    return env->intern(env, "t");
}

// --- Module Function: Poll Events ---
// Lisp calls this function. We now have a valid `env`.
static emacs_value Fime_hook_poll(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data) {
    // args[0] should be the hook function to call
    if (nargs < 1) return env->intern(env, "nil");
    emacs_value hook_func = args[0];

    pthread_mutex_lock(&queueMutex);
    NSArray *currentEvents = [eventQueue copy];
    [eventQueue removeAllObjects];
    pthread_mutex_unlock(&queueMutex);

    if ([currentEvents count] == 0) {
        return env->intern(env, "nil");
    }

    // Process queue
    for (NSDictionary *evt in currentEvents) {
        long keyCode = [evt[@"keyCode"] longValue];
        unsigned long mods = [evt[@"modifiers"] unsignedLongValue];

        // Convert C values to Lisp values
        emacs_value lisp_keycode = env->make_integer(env, keyCode);
        emacs_value lisp_mods = env->make_integer(env, mods);

        // Call the Lisp hook: (funcall hook-func keycode modifiers)
        emacs_value func_args[] = { hook_func, lisp_keycode, lisp_mods };
        env->funcall(env, env->intern(env, "funcall"), 3, func_args);
    }

    return env->make_integer(env, [currentEvents count]);
}

// --- Module Function: Get Input Source ---
static emacs_value Fime_hook_get_input_source(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data) {
    TISInputSourceRef currentSource = TISCopyCurrentKeyboardInputSource();
    if (!currentSource) {
        return env->intern(env, "nil");
    }
    
    NSString *sourceID = (__bridge NSString *)TISGetInputSourceProperty(currentSource, kTISPropertyInputSourceID);
    // TISCopyCurrentKeyboardInputSource returns a retained object, so we must release it.
    // However, TISGetInputSourceProperty returns a non-retained reference (Get rule), 
    // so we don't release sourceID, but we do release currentSource.
    // Wait, sourceID is an NSString* bridged from CFStringRef.
    
    emacs_value result;
    if (sourceID) {
        result = env->make_string(env, [sourceID UTF8String], [sourceID lengthOfBytesUsingEncoding:NSUTF8StringEncoding]);
    } else {
        result = env->intern(env, "nil");
    }
    
    CFRelease(currentSource);
    return result;
}

// --- Module Function: Set Input Source ---
static emacs_value Fime_hook_set_input_source(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data) {
    if (nargs < 1) return env->intern(env, "nil");
    
    ptrdiff_t len;
    env->copy_string_contents(env, args[0], NULL, &len);
    char *buffer = malloc(len);
    env->copy_string_contents(env, args[0], buffer, &len);
    NSString *targetID = [NSString stringWithUTF8String:buffer];
    free(buffer);
    
    if (!targetID) {
         return env->intern(env, "nil");
    }

    NSDictionary *filter = @{ (__bridge NSString *)kTISPropertyInputSourceID : targetID };
    CFArrayRef sources = TISCreateInputSourceList((__bridge CFDictionaryRef)filter, false);
    
    if (!sources) {
        return env->intern(env, "nil");
    }
    
    if (CFArrayGetCount(sources) == 0) {
        CFRelease(sources);
        return env->intern(env, "nil");
    }
    
    TISInputSourceRef source = (TISInputSourceRef)CFArrayGetValueAtIndex(sources, 0);
    OSStatus status = TISSelectInputSource(source);
    CFRelease(sources);
    
    return (status == noErr) ? env->intern(env, "t") : env->intern(env, "nil");
}

// --- Initialization ---
int emacs_module_init(struct emacs_runtime *ert) {
    emacs_env *env = ert->get_environment(ert);

    // Define fset helper
    emacs_value fset = env->intern(env, "fset");
    
    // Register `ime-hook-internal-start`
    emacs_value func_start = env->make_function(env, 0, 0, Fime_hook_start, "Start the NSEvent monitor.", NULL);
    emacs_value sym_start = env->intern(env, "ime-hook-internal-start");
    emacs_value args_start[] = { sym_start, func_start };
    env->funcall(env, fset, 2, args_start);

    // Register `ime-hook-internal-stop`
    emacs_value func_stop = env->make_function(env, 0, 0, Fime_hook_stop, "Stop the NSEvent monitor.", NULL);
    emacs_value sym_stop = env->intern(env, "ime-hook-internal-stop");
    emacs_value args_stop[] = { sym_stop, func_stop };
    env->funcall(env, fset, 2, args_stop);

    // Register `ime-hook-internal-poll`
    emacs_value func_poll = env->make_function(env, 1, 1, Fime_hook_poll, "Poll queued events and call HOOK-FUNC.", NULL);
    emacs_value sym_poll = env->intern(env, "ime-hook-internal-poll");
    emacs_value args_poll[] = { sym_poll, func_poll };
    env->funcall(env, fset, 2, args_poll);

    // Register `ime-hook-internal-get-input-source`
    emacs_value func_get_source = env->make_function(env, 0, 0, Fime_hook_get_input_source, "Get the current input source ID.", NULL);
    emacs_value sym_get_source = env->intern(env, "ime-hook-internal-get-input-source");
    emacs_value args_get_source[] = { sym_get_source, func_get_source };
    env->funcall(env, fset, 2, args_get_source);

    // Register `ime-hook-internal-set-input-source`
    emacs_value func_set_source = env->make_function(env, 1, 1, Fime_hook_set_input_source, "Set the current input source by ID.", NULL);
    emacs_value sym_set_source = env->intern(env, "ime-hook-internal-set-input-source");
    emacs_value args_set_source[] = { sym_set_source, func_set_source };
    env->funcall(env, fset, 2, args_set_source);

    // Provide the feature
    emacs_value provide = env->intern(env, "provide");
    emacs_value feature = env->intern(env, "ime-hook-module");
    emacs_value provide_args[] = { feature };
    env->funcall(env, provide, 1, provide_args);

    return 0;
}
