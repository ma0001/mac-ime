;;; mac-ime-test.el --- Tests for mac-ime -*- lexical-binding: t; -*-

(require 'ert)
(require 'mac-ime)

;; Add test directory to load-path to find mac-ime-mock
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'mac-ime-mock)

;; Enable mock
(mac-ime-mock-enable)

(defun mac-ime-test-reset ()
  "Reset both mock and mac-ime internal state."
  (mac-ime-mock-reset)
  (setq mac-ime--saved-input-source nil
        mac-ime--sync-paused nil
        mac-ime--expected-input-source nil
        mac-ime--current-input-source nil
        mac-ime-last-on-input-source nil
        mac-ime-last-off-input-source nil
        mac-ime--ignore-input-source-change nil
        current-input-method nil
        mac-ime-functions nil
        mac-ime-debug-level 2))

(ert-deftest mac-ime-mock-basic-test ()
  "Test basic mock functionality."
  (mac-ime-test-reset)
  (should (equal (mac-ime-internal-start) t))
  (should mac-ime-mock-running)
  (should (equal (mac-ime-internal-stop) t))
  (should-not mac-ime-mock-running))

(ert-deftest mac-ime-mock-input-source-test ()
  "Test input source get/set with mock."
  (mac-ime-test-reset)
  (should (equal (mac-ime-internal-get-input-source) "com.apple.keylayout.US"))
  (should (mac-ime-internal-set-input-source "com.apple.inputmethod.Kotoeri.RomajiTyping"))
  (should (equal (mac-ime-internal-get-input-source) "com.apple.inputmethod.Kotoeri.RomajiTyping"))
  (should-not (mac-ime-internal-set-input-source "invalid.source")))

(ert-deftest mac-ime-event-handling-test ()
  "Test event handling via poll."
  (mac-ime-test-reset)
  (mac-ime-internal-start)
  
  (let ((called nil))
    (add-hook 'mac-ime-functions (lambda (k m) (setq called (list k m))))
    
    ;; Simulate 'x' key (keycode 7) with no modifiers
    (mac-ime-mock-simulate-event 7 0)
    
    ;; Poll should trigger the hook
    (mac-ime-poll)
    
    (should (equal called '(7 0)))))

(ert-deftest mac-ime-auto-deactivate-on-prefix-test ()
  "Test automatic IME deactivation on prefix key."
  (mac-ime-test-reset)
  ;; Setup: IME is ON
  (mac-ime-internal-set-input-source "com.apple.inputmethod.Kotoeri.RomajiTyping")
  (setq current-input-method mac-ime-input-method)
  
  ;; Configure prefix key: C-j (keycode 38, control modifier)
  (defconst mac-ime-kVK_ANSI_J 38)
  (let ((mac-ime-prefix-keys `((,mac-ime-kVK_ANSI_J . ,mac-ime-NSEventModifierFlagControl)))
        (mac-ime-ime-off-input-source "com.apple.keylayout.US"))
    
    (add-hook 'mac-ime-functions #'mac-ime-deactivate-ime-on-prefix)
    
    ;; Simulate C-j
    (mac-ime-mock-simulate-event mac-ime-kVK_ANSI_J mac-ime-NSEventModifierFlagControl)
    
    ;; Poll
    (mac-ime-poll)
    
    ;; Check if IME was deactivated (input source changed to US)
    (should (equal (mac-ime-internal-get-input-source) "com.apple.keylayout.US"))
    
    ;; Run pre-command-hook to restore IME
    (run-hooks 'pre-command-hook)
    
    ;; Check if IME was restored
    (should (equal (mac-ime-internal-get-input-source) "com.apple.inputmethod.Kotoeri.RomajiTyping"))))

(ert-deftest mac-ime-auto-deactivate-functions-test ()
  "Test automatic IME deactivation for specific functions."
  (mac-ime-test-reset)
  ;; Setup: IME is ON
  (mac-ime-internal-set-input-source "com.apple.inputmethod.Kotoeri.RomajiTyping")
  (setq current-input-method mac-ime-input-method)
  (setq mac-ime-last-off-input-source "com.apple.keylayout.US")
  
  (let ((inner-source nil))
    (defun mac-ime-test-func ()
      (setq inner-source (mac-ime-internal-get-input-source)))
    
    ;; Register function
    (mac-ime-auto-deactivate 'mac-ime-test-func)
    
    ;; Call function
    (mac-ime-test-func)
    
    ;; Check if IME was deactivated inside the function
    (should (equal inner-source "com.apple.keylayout.US"))
    
    ;; Check if IME was restored after the function
    (should (equal (mac-ime-internal-get-input-source) "com.apple.inputmethod.Kotoeri.RomajiTyping"))
    
    ;; Cleanup advice
    (advice-remove 'mac-ime-test-func #'mac-ime--auto-deactivate-advice)))

(ert-deftest mac-ime-sync-state-test ()
  "Test synchronization of Emacs input method state with OS input source."
  (mac-ime-test-reset)
  
  ;; Case 1: OS changes to Japanese -> Emacs should activate mac-ime
  (mac-ime-internal-set-input-source "com.apple.inputmethod.Kotoeri.RomajiTyping")
  
  ;; Trigger sync (activates IME, sets paused=t, expected=RomajiTyping)
  (mac-ime--sync-input-method)
  
  (should (equal current-input-method mac-ime-input-method))
  
  ;; Resolve paused state
  ;; The mock set-input-source was called by activate-ime, so current source is RomajiTyping.
  ;; Calling sync again should match expected source and clear paused.
  (mac-ime--sync-input-method)
  (should-not mac-ime--sync-paused)
  
  ;; Case 2: OS changes to US -> Emacs should deactivate mac-ime
  (mac-ime-internal-set-input-source "com.apple.keylayout.US")
  (mac-ime--sync-input-method)
  
  (should (equal current-input-method nil)))

(provide 'mac-ime-mock-test)
