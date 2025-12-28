;;; mac-ime.el --- NSEvent hook for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Masami
;; Author: Masami Iwata
;; Version: 0.1.0
;; Keywords: mac, input, ime
;; Package-Requires: ((emacs "27.1"))
;; URL: https://github.com/ma0001/mac-ime

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package provides a way to hook into macOS global key events
;; using a dynamic module.  It is intended to be used for IME integration.

;;; Code:

(require 'cl-lib)
(require 'nadvice)

(declare-function mac-ime-internal-get-input-source-list nil ())
(declare-function mac-ime-internal-get-input-source nil ())
(declare-function mac-ime-internal-set-input-source nil (source-id))
(declare-function mac-ime-internal-poll nil (hook-func))
(declare-function mac-ime-internal-start nil ())
(declare-function mac-ime-internal-stop nil ())

(defconst mac-ime-input-method "mac-ime"
  "Name of the mac-ime input method.")

(defvar mac-ime-module-file "mac-ime-module.so"
  "Name of the dynamic module file.")

(defvar mac-ime-module-path
  (expand-file-name mac-ime-module-file
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Full path to the dynamic module.")

(defvar mac-ime-timer nil
  "Timer object for polling events.")

(defvar mac-ime-poll-interval 0.05
  "Interval in seconds to poll for events.")

(defcustom mac-ime-functions nil
  "List of functions to call when a key event occurs.
Each function is called with two arguments: (KEYCODE MODIFIERS)."
  :type 'hook
  :group 'mac-ime)

(defconst mac-ime-kVK_ANSI_S 1 "Virtual key code for `s'.")
(defconst mac-ime-kVK_ANSI_X 7 "Virtual key code for `x'.")
(defconst mac-ime-kVK_ANSI_C 8 "Virtual key code for `c'.")
(defconst mac-ime-kVK_ANSI_H 4 "Virtual key code for `h'.")
(defconst mac-ime-kVK_ANSI_G 5 "Virtual key code for `g'.")
(defconst mac-ime-kVK_Escape 53 "Virtual key code for `Escape'.")
(defconst mac-ime-NSEventModifierFlagCmd #x100108 "Modifier flag for Cmd key.")
(defconst mac-ime-NSEventModifierFlagRightCmd #x100110 "Modifier flag for Right Cmd key.")
(defconst mac-ime-NSEventModifierFlagControl #x40101 "Modifier flag for Control key.")
(defconst mac-ime-NSEventModifierFlagRightControl #x42100 "Modifier flag for Right Control key.")
(defconst mac-ime-NSEventModifierFlagOption #x80120 "Modifier flag for Option key.")
(defconst mac-ime-NSEventModifierFlagRightOption #x80140 "Modifier flag for Right Option key.")
(defconst mac-ime-NSEventModifierFlagFunction #x800100 "Modifier flag for Function key.")

(defcustom mac-ime-prefix-keys nil
  "Alist of prefix keys that trigger IME deactivation.
Each element is a cons cell (KEYCODE . MODIFIERS).
If nil, it is automatically configured based on mac-*-modifier variables."
  :type '(alist :key-type integer :value-type integer)
  :group 'mac-ime)

(defvar mac-ime-modifier-action-table
  '((control . (mac-ime-kVK_ANSI_X mac-ime-kVK_ANSI_C mac-ime-kVK_ANSI_H))
    (meta . (mac-ime-kVK_ANSI_G mac-ime-kVK_ANSI_S))
    (nomodifier . (mac-ime-kVK_Escape)))
  "Table mapping modifier values to list of key codes to register.")

(defun mac-ime-resolve-modifier-value (modifier-var)
  "Resolve the value of MODIFIER-VAR, handling `left' inheritance."
  (let ((val (if (boundp modifier-var) (symbol-value modifier-var) nil)))
    (if (eq val 'left)
        (let ((base-var-name (replace-regexp-in-string "-right-" "-" (symbol-name modifier-var))))
          (let ((base-var (intern base-var-name)))
            (if (boundp base-var)
                (symbol-value base-var)
              val)))
      val)))

(defvar mac-ime--generated-prefix-keys nil
  "Cache for generated prefix keys.")

(defun mac-ime-generate-prefix-keys ()
  "Generate prefix keys based on mac modifier settings."
  (or mac-ime--generated-prefix-keys
      (let ((keys '())
            (modifier-vars
             `((mac-command-modifier . ,mac-ime-NSEventModifierFlagCmd)
               (mac-right-command-modifier . ,mac-ime-NSEventModifierFlagRightCmd)
               (mac-control-modifier . ,mac-ime-NSEventModifierFlagControl)
               (mac-right-control-modifier . ,mac-ime-NSEventModifierFlagRightControl)
               (mac-option-modifier . ,mac-ime-NSEventModifierFlagOption)
               (mac-right-option-modifier . ,mac-ime-NSEventModifierFlagRightOption)
               (mac-function-modifier . ,mac-ime-NSEventModifierFlagFunction))))
        (dolist (entry modifier-vars)
          (let* ((var (car entry))
                 (flag (cdr entry))
                 (val (mac-ime-resolve-modifier-value var))
                 (key-codes (cdr (assoc val mac-ime-modifier-action-table))))
            (dolist (code-sym key-codes)
              (let ((code (symbol-value code-sym)))
                (push (cons code flag) keys)))))
        ;; Add default keys (modifier 0)
        (let ((default-keys (cdr (assoc 'nomodifier mac-ime-modifier-action-table))))
          (dolist (code-sym default-keys)
            (let ((code (symbol-value code-sym)))
              (push (cons code 0) keys))))
        (setq mac-ime--generated-prefix-keys keys))))

(defcustom mac-ime-no-ime-input-source-regexp "\\(keylayout\\|roman\\)"
  "Regexp matching input source IDs that indicate IME is off.
Case is ignored."
  :type 'regexp
  :group 'mac-ime)

(defcustom mac-ime-ime-off-input-source nil
  "Input source ID to switch to when a prefix key is pressed (to turn off IME).
If nil, `mac-ime-last-off-input-source` or the first input source matching
`mac-ime-no-ime-input-source-regexp` will be used."
  :type '(choice (const :tag "Auto-detect" nil)
                 (string :tag "Input Source ID"))
  :group 'mac-ime)

(defcustom mac-ime-ime-on-input-source nil
  "Input source ID to switch to when activating IME.
If nil, `mac-ime-last-on-input-source` or the first input source NOT matching
`mac-ime-no-ime-input-source-regexp` will be used."
  :type '(choice (const :tag "Auto-detect" nil)
                 (string :tag "Input Source ID"))
  :group 'mac-ime)

(defcustom mac-ime-auto-deactivate-functions '((read-string . 4)
                                               (read-char . 1)
                                               (read-event . 1)
                                               (read-char-exclusive . 1)
                                               (read-char-choice . 2)
                                               (read-no-blanks-input . 2)
                                               (read-from-minibuffer . 6)
                                               (completing-read . 7)
                                               y-or-n-p
                                               yes-or-no-p
                                               map-y-or-n-p)
  "List of functions to automatically deactivate IME during execution.
Each element can be a function symbol or a cons cell (FUNCTION . ARG-INDEX).
If it is a cons cell, ARG-INDEX specifies the position of the INHERIT-INPUT-METHOD argument.
If the argument is non-nil and the current input method is `mac-ime-input-method`,
IME will remain active.  Otherwise, IME is deactivated."
  :type '(repeat (choice function (cons function integer)))
  :group 'mac-ime)

(defcustom mac-ime-temporary-deactivate-functions '(universal-argument--mode)
  "List of functions to temporarily deactivate IME before execution.
The IME state is restored in `pre-command-hook`.
 
Note: `universal-argument--mode` is used instead of `universal-argument`
because `universal-argument` is only called once.  `universal-argument--mode`
is called by `universal-argument`, `universal-argument-more`, and
`digit-argument`, ensuring IME is deactivated for the entire sequence."
  :type '(repeat function)
  :group 'mac-ime)

(defcustom mac-ime-debug-level 0
  "Debug level for mac-ime.
0: No debug messages.
1: Output input keys.
2: Output function execution messages."
  :type 'integer
  :group 'mac-ime)

(defcustom mac-ime-title-rules
  '(("romajityping" . "[あ]")
    ("kanatyping" . "[かな]")
    (t . "[IME]"))
  "Alist of rules to determine the input method title based on the input source ID.
Each element is a cons cell (REGEXP . TITLE).  The input source ID is matched
against REGEXP (case-insensitive).  If REGEXP is t, it matches any input source
and serves as a default.  The first matching rule determines the title."
  :type '(alist :key-type (choice (string :tag "Regexp") (const :tag "Default" t))
                :value-type string)
  :group 'mac-ime)

(defvar mac-ime-last-on-input-source nil
  "The last used input source ID for IME ON.")

(defvar mac-ime-last-off-input-source nil
  "The last used input source ID for IME OFF.")

(defvar mac-ime--current-input-source nil
  "Cache of the current input source ID.")

(defvar mac-ime--ignore-input-source-change nil
  "If non-nil, `mac-ime--check-input-source-change` skips updates.
The last input source will not be updated.")

(defvar mac-ime--saved-input-source nil
  "Saved input source ID to restore.")

(defun mac-ime--debug (level format-string &rest args)
  "Output a debug message if `mac-ime-debug-level` is >= LEVEL.
FORMAT-STRING and ARGS are passed to `message`."
  (when (>= mac-ime-debug-level level)
    (let ((timestamp (format-time-string "%M:%S.%3N")))
      (apply #'message (concat (format "[%s] mac-ime [DEBUG]: " timestamp) format-string) args))))

(defun mac-ime--get-ime-off-input-source ()
  "Return the input source ID to use to turn off IME.
If `mac-ime-ime-off-input-source` is non-nil, return it.
Otherwise, use `mac-ime-last-off-input-source`.
If that is also nil, find the first input source matching
`mac-ime-no-ime-input-source-regexp` and cache it."
  (or mac-ime-ime-off-input-source
      mac-ime-last-off-input-source
      (setq mac-ime-last-off-input-source
            (cl-loop for source in (mac-ime-get-input-source-list)
                     if (let ((case-fold-search t))
                          (string-match-p mac-ime-no-ime-input-source-regexp source))
                     return source))))

(defun mac-ime--get-ime-on-input-source ()
  "Return the input source ID to use to turn on IME.
If `mac-ime-ime-on-input-source` is non-nil, return it.
Otherwise, use `mac-ime-last-on-input-source`.
If that is also nil, find the first input source NOT matching
`mac-ime-no-ime-input-source-regexp` and cache it."
  (or mac-ime-ime-on-input-source
      mac-ime-last-on-input-source
      (setq mac-ime-last-on-input-source
            (cl-loop for source in (mac-ime-get-input-source-list)
                     if (not (let ((case-fold-search t))
                               (string-match-p mac-ime-no-ime-input-source-regexp source)))
                     return source))))

(defun mac-ime--restore-input-source ()
  "Restore the saved input source."
  (mac-ime--debug 2 "mac-ime--restore-input-source")
  (when mac-ime--saved-input-source
    (mac-ime-set-input-source mac-ime--saved-input-source)
    (setq mac-ime--saved-input-source nil))
  (setq mac-ime--ignore-input-source-change nil)
  (remove-hook 'pre-command-hook #'mac-ime--restore-input-source))

(defun mac-ime-deactivate-ime-temporarily ()
  "Deactivate IME temporarily.
The original input source is restored in `pre-command-hook`."
  (mac-ime--debug 2 "mac-ime-deactivate-ime-temporarily")
  (when (and (not mac-ime--saved-input-source)
             (equal current-input-method mac-ime-input-method))
    (let ((source (mac-ime--get-ime-off-input-source))
          (current (mac-ime-get-input-source)))
      (when (and source current (not (string= source current)))
        (setq mac-ime--saved-input-source current)
        (setq mac-ime--ignore-input-source-change t)
        (mac-ime-set-input-source source)
        (add-hook 'pre-command-hook #'mac-ime--restore-input-source)))))

(defun mac-ime-deactivate-ime-on-prefix (keycode modifiers)
  "Deactivate IME when a prefix key defined in `mac-ime-prefix-keys` is pressed.
This function is intended to be added to `mac-ime-functions`.
KEYCODE is the virtual key code.
MODIFIERS is the modifier flags."
  (when (and (not mac-ime--saved-input-source)
             (equal current-input-method mac-ime-input-method))
    (let ((prefix-keys (or mac-ime-prefix-keys (mac-ime-generate-prefix-keys))))
      (cl-loop for (k . m) in prefix-keys
               if (and (= keycode k)
                       (= (logand modifiers m) m))
               return (mac-ime-deactivate-ime-temporarily)))))

(defun mac-ime--load-module ()
  "Load the dynamic module if not already loaded."
  (unless (featurep 'mac-ime-module)
    (if (file-exists-p mac-ime-module-path)
        (module-load mac-ime-module-path)
      (message "mac-ime: Module not found at %s." mac-ime-module-path))))

(defvar mac-ime--last-selected-buffer nil
  "The buffer that was current during the last window selection change.")

(defun mac-ime-handler (keycode modifiers)
  "Internal handler called by the C module.
Calls functions in `mac-ime-functions`.
KEYCODE is the virtual key code.
MODIFIERS is the modifier flags."
  (mac-ime--debug 1 "Key event: keycode=%d, modifiers=%d" keycode modifiers)
  (run-hook-with-args 'mac-ime-functions keycode modifiers)
  ;; Skip synchronization if the buffer has changed recently.
  ;; This prevents race conditions where the poll runs before window-selection-change-functions.
  (let ((current (current-buffer)))
    (when (eq current mac-ime--last-selected-buffer)
      (mac-ime--check-input-source-change)
      (mac-ime--sync-input-method))))
  

(defun mac-ime--check-input-source-change ()
  "Check if input source has changed and update last used input sources.
Updates `mac-ime-last-on-input-source` and `mac-ime-last-off-input-source`.
Input sources matching `mac-ime-no-ime-input-source-regexp` are saved to
off-source, others to on-source."
  (unless mac-ime--ignore-input-source-change
    (let ((current (mac-ime-get-input-source)))
      (when (and current
                 mac-ime--current-input-source
                 (not (string= current mac-ime--current-input-source)))
        (let ((case-fold-search t))
          (if (string-match-p mac-ime-no-ime-input-source-regexp mac-ime--current-input-source)
              (setq mac-ime-last-off-input-source mac-ime--current-input-source)
            (setq mac-ime-last-on-input-source mac-ime--current-input-source))))
      (setq mac-ime--current-input-source current))))

(defun mac-ime-poll ()
  "Poll the C module for events."
  (when (featurep 'mac-ime-module)
    (mac-ime-internal-poll #'mac-ime-handler)))

(defun mac-ime-activate-input-method (input-method)
  "Activate the mac-ime input method."
  (mac-ime--debug 2 "mac-ime-activate-input-method called in %s buffer %s" input-method (current-buffer))
  (mac-ime-activate-ime)
  (setq deactivate-current-input-method-function #'mac-ime-deactivate-ime)
  (when-let ((source (mac-ime-get-input-source)))
    (mac-ime--update-title source)))

(register-input-method mac-ime-input-method "Japanese" 'mac-ime-activate-input-method "[こ]" "macOS System IME")

(defun mac-ime-update-state (&optional _window)
  "Update IME state based on the current input method.
Activate IME if `current-input-method` is `mac-ime-input-method`.
Otherwise, deactivate IME."
  (mac-ime--debug 2 "mac-ime-update-state: current-input-method=%s buffer=%s" current-input-method (current-buffer))
  (setq mac-ime--last-selected-buffer (current-buffer))
  (unless mac-ime--ignore-input-source-change
    (if (equal current-input-method mac-ime-input-method)
        (mac-ime-activate-ime)
      (mac-ime-deactivate-ime))))

;;;###autoload
(defun mac-ime-enable ()
  "Enable the global key monitor."
  (interactive)
  (mac-ime--debug 2 "mac-ime-enable called")
  (mac-ime--load-module)
  (when (featurep 'mac-ime-module)
    (mac-ime-internal-start)
    (unless mac-ime-timer
      (setq mac-ime-timer (run-with-timer 0 mac-ime-poll-interval #'mac-ime-poll))
      (add-hook 'mac-ime-functions #'mac-ime-deactivate-ime-on-prefix)
      (dolist (func mac-ime-auto-deactivate-functions)
        (mac-ime-auto-deactivate func))
      (dolist (func mac-ime-temporary-deactivate-functions)
        (mac-ime-temporary-deactivate func))
      (add-hook 'window-selection-change-functions #'mac-ime-update-state)
      (add-function :after after-focus-change-function #'mac-ime--on-focus)
      (message "mac-ime enabled."))))

;;;###autoload
(defun mac-ime-disable ()
  "Disable the global key monitor."
  (interactive)
  (mac-ime--debug 2 "mac-ime-disable called")
  (when mac-ime-timer
    (cancel-timer mac-ime-timer)
    (setq mac-ime-timer nil))
  (when (featurep 'mac-ime-module)
    (remove-hook 'mac-ime-functions #'mac-ime-deactivate-ime-on-prefix)
    (mac-ime-internal-stop)
    (dolist (func mac-ime-auto-deactivate-functions)
      (let* ((f-sym (if (consp func) (car func) func))
             (advice-name (intern (format "mac-ime--auto-deactivate-%s" f-sym))))
        (advice-remove f-sym advice-name)))
    (dolist (func mac-ime-temporary-deactivate-functions)
      (advice-remove func #'mac-ime--temporary-deactivate-advice))
    (remove-function after-focus-change-function #'mac-ime--on-focus)
    (remove-hook 'window-selection-change-functions #'mac-ime-update-state)
    (message "mac-ime disabled.")))

;;;###autoload
(defun mac-ime-get-input-source ()
  "Get the current input source ID."
  (mac-ime--load-module)
  (when (featurep 'mac-ime-module)
    (mac-ime-internal-get-input-source)))

;;;###autoload
(defun mac-ime-set-input-source (source-id)
  "Set the current input source to SOURCE-ID."
  (mac-ime--debug 2 "mac-ime-set-input-source: %s" source-id)
  (mac-ime--load-module)
  (when (featurep 'mac-ime-module)
    (mac-ime-internal-set-input-source source-id)))

;;;###autoload
(defun mac-ime-get-input-source-list ()
  "Get a list of all selectable input source IDs."
  (mac-ime--load-module)
  (when (featurep 'mac-ime-module)
    (mac-ime-internal-get-input-source-list)))

(defun mac-ime--auto-deactivate-body (orig-fun args config)
  "Body of the auto-deactivate advice.
ORIG-FUN is the original function.
ARGS are the arguments.
CONFIG is the configuration (symbol or cons)."
  (mac-ime--debug 2 "mac-ime--auto-deactivate-body called with config %s" config)
  (let* ((inherit-index (if (consp config) (cdr config) nil))
         (should-inherit (and inherit-index (nth inherit-index args)))
         (should-deactivate
          (if should-inherit
              (not (equal current-input-method mac-ime-input-method))
            t)))
    (if should-deactivate
        (let ((saved-source (mac-ime-get-input-source))
              (off-source (mac-ime--get-ime-off-input-source)))
          (if (and off-source saved-source)
              (progn
                (setq mac-ime--ignore-input-source-change t)
                (mac-ime-set-input-source off-source)
                (unwind-protect
                    (apply orig-fun args)
                  (mac-ime--debug 2 "mac-ime--auto-deactivate-body Restoring input source to %s" saved-source)
                  (mac-ime-set-input-source saved-source)
                  (setq mac-ime--ignore-input-source-change nil)))
            (apply orig-fun args)))
      (apply orig-fun args))))

;;;###autoload
(defun mac-ime-auto-deactivate (func)
  "Add advice to FUNC to deactivate IME during its execution.
The IME state is restored after FUNC completes."
  (let* ((f-sym (if (consp func) (car func) func))
         (advice-name (intern (format "mac-ime--auto-deactivate-%s" f-sym))))
    (fset advice-name
          (lambda (orig-fun &rest args)
            (let ((current-config (cl-find f-sym mac-ime-auto-deactivate-functions
                                           :test (lambda (f item)
                                                   (eq f (if (consp item) (car item) item))))))
              (mac-ime--auto-deactivate-body orig-fun args (or current-config f-sym)))))
    (advice-add f-sym :around advice-name)))

(defun mac-ime--temporary-deactivate-advice (&rest _args)
  "Advice to deactivate IME temporarily."
  (mac-ime-deactivate-ime-temporarily))

;;;###autoload
(defun mac-ime-temporary-deactivate (func)
  "Add advice to FUNC to deactivate IME temporarily before its execution."
  (advice-add func :before #'mac-ime--temporary-deactivate-advice))

(defvar mac-ime--sync-paused nil
  "Whether input method synchronization is paused.")

(defvar mac-ime--expected-input-source nil
  "The expected input source ID when synchronization is paused.")

(defun mac-ime--update-title (input-source)
  "Update `current-input-method-title`.
based on INPUT-SOURCE and `mac-ime-title-rules`."
  (let ((title (cl-loop for (regexp . t-str) in mac-ime-title-rules
                        if (or (eq regexp t)
                               (and (stringp regexp)
                                    (let ((case-fold-search t))
                                      (string-match-p regexp input-source))))
                        return t-str)))
    (when title
      (setq current-input-method-title title)
      (force-mode-line-update))))

(defun mac-ime--on-focus ()
  "Handler for focus change.
Resets sync state and synchronizes input method."
  (when (frame-focus-state)
    (mac-ime--debug 2 "mac-ime--on-focus called")
    (setq mac-ime--sync-paused nil
          mac-ime--expected-input-source nil)
    (mac-ime--sync-input-method)))

(defun mac-ime--sync-input-method ()
  "Synchronize `current-input-method` with the macOS input source."
  (unless mac-ime--saved-input-source
    (let ((current-source (mac-ime-get-input-source)))
      (when current-source
        (if mac-ime--sync-paused
            (when (and mac-ime--expected-input-source
                       (string= current-source mac-ime--expected-input-source))
              (mac-ime--debug 2 "mac-ime--sync-input-method: sync resumed (reached expected source: %s)" current-source)
              (setq mac-ime--sync-paused nil
                    mac-ime--expected-input-source nil)
              (when (equal current-input-method mac-ime-input-method)
                (mac-ime--update-title current-source)))
          (let ((case-fold-search t))
            (if (string-match-p mac-ime-no-ime-input-source-regexp current-source)
                (when (equal current-input-method mac-ime-input-method)
                  (mac-ime--debug 2 "mac-ime--sync-input-method: deactivating input method (source: %s buffer=%s)" current-source (current-buffer))
                  (deactivate-input-method))
              (unless (equal current-input-method mac-ime-input-method)
                (mac-ime--debug 2 "mac-ime--sync-input-method: activating input method (source: %s) buffer=%s" current-source (current-buffer))
                (activate-input-method mac-ime-input-method))
              (when (equal current-input-method mac-ime-input-method)
                (mac-ime--update-title current-source)))))))))

;;;###autoload
(defun mac-ime-activate-ime ()
  "Activate the IME input source.
Uses `mac-ime--get-ime-on-input-source` to determine the input source."
  (interactive)
  (let ((source (mac-ime--get-ime-on-input-source)))
    (mac-ime--debug 2 "mac-ime-activate-ime: source=%s buffer=%s" source (current-buffer))
    (when source
      (mac-ime-set-input-source source)
      (setq mac-ime--sync-paused t
            mac-ime--expected-input-source source))))

;;;###autoload
(defun mac-ime-deactivate-ime ()
  "Deactivate the IME input source.
Uses `mac-ime--get-ime-off-input-source` to determine the input source."
  (interactive)
  (let ((source (mac-ime--get-ime-off-input-source)))
    (mac-ime--debug 2 "mac-ime-deactivate-ime: source=%s buffer=%s" source (current-buffer))
    (when source
      (mac-ime-set-input-source source)
      (setq mac-ime--sync-paused t
            mac-ime--expected-input-source source))))
      
(provide 'mac-ime)
;;; mac-ime.el ends here
