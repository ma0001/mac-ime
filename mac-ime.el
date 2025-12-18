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
;; using a dynamic module. It is intended to be used for IME integration.

;;; Code:

(require 'cl-lib)

(defvar mac-ime-module-file "mac-ime-module.so"
  "Name of the dynamic module file.")

(defvar mac-ime-module-path
  (expand-file-name mac-ime-module-file (file-name-directory (or load-file-name buffer-file-name)))
  "Full path to the dynamic module.")

(defvar mac-ime-timer nil
  "Timer object for polling events.")

(defvar mac-ime-poll-interval 0.05
  "Interval in seconds to poll for events.")

(defcustom mac-ime-functions nil
  "List of functions to call when a key event occurs.
Each function is called with two arguments: (keycode modifiers)."
  :type 'hook
  :group 'mac-ime)

(defconst mac-ime-kVK_ANSI_X 7 "Virtual key code for 'x'.")
(defconst mac-ime-kVK_ANSI_C 8 "Virtual key code for 'c'.")
(defconst mac-ime-kVK_ANSI_H 4 "Virtual key code for 'h'.")
(defconst mac-ime-kVK_ANSI_G 5 "Virtual key code for 'g'.")
(defconst mac-ime-kVK_Escape 53 "Virtual key code for 'Escape'.")
(defconst mac-ime-NSEventModifierFlagControl 262401 "Modifier flag for Control key.")
(defconst mac-ime-NSEventModifierFlagCmd 1048840 "Modifier flag for Cmd key.")

(defcustom mac-ime-prefix-keys
  `((,mac-ime-kVK_ANSI_X . ,mac-ime-NSEventModifierFlagControl)
    (,mac-ime-kVK_ANSI_C . ,mac-ime-NSEventModifierFlagControl)
    (,mac-ime-kVK_ANSI_H . ,mac-ime-NSEventModifierFlagControl)
    (,mac-ime-kVK_ANSI_G . ,mac-ime-NSEventModifierFlagCmd)
    (,mac-ime-kVK_Escape . 0))
  "Alist of prefix keys that trigger IME deactivation.
Each element is a cons cell (KEYCODE . MODIFIERS).
If the keycode matches and the specified modifiers are set, IME is deactivated."
  :type '(alist :key-type integer :value-type integer)
  :group 'mac-ime)

(defcustom mac-ime-ime-off-input-source nil
  "Input source ID to switch to when a prefix key is pressed (to turn off IME).
If nil, `mac-ime-last-off-input-source` or the first input source containing 'keylayout' in its ID will be used."
  :type '(choice (const :tag "Auto-detect" nil)
                 (string :tag "Input Source ID"))
  :group 'mac-ime)

(defcustom mac-ime-ime-on-input-source nil
  "Input source ID to switch to when activating IME.
If nil, `mac-ime-last-on-input-source` or the first input source containing 'inputmethod' will be used."
  :type '(choice (const :tag "Auto-detect" nil)
                 (string :tag "Input Source ID"))
  :group 'mac-ime)

(defcustom mac-ime-auto-deactivate-functions '(universal-argument read-string read-char read-from-minibuffer y-or-n-p yes-or-no-p map-y-or-n-p)
  "List of functions to automatically deactivate IME during execution."
  :type '(repeat function)
  :group 'mac-ime)

(defun mac-ime--get-ime-off-input-source ()
  "Return the input source ID to use to turn off IME.
If `mac-ime-ime-off-input-source` is non-nil, return it.
Otherwise, use `mac-ime-last-off-input-source`.
If that is also nil, find the first input source containing 'keylayout' and cache it."
  (or mac-ime-ime-off-input-source
      mac-ime-last-off-input-source
      (setq mac-ime-last-off-input-source
            (cl-loop for source in (mac-ime-get-input-source-list)
                     if (string-match-p "keylayout" source)
                     return source))))

(defun mac-ime--get-ime-on-input-source ()
  "Return the input source ID to use to turn on IME.
If `mac-ime-ime-on-input-source` is non-nil, return it.
Otherwise, use `mac-ime-last-on-input-source`.
If that is also nil, find the first input source containing 'inputmethod' and cache it."
  (or mac-ime-ime-on-input-source
      mac-ime-last-on-input-source
      (setq mac-ime-last-on-input-source
            (cl-loop for source in (mac-ime-get-input-source-list)
                     if (string-match-p "inputmethod" source)
                     return source))))

(defvar mac-ime--saved-input-source nil
  "Saved input source ID to restore.")

(defun mac-ime--restore-input-source ()
  "Restore the saved input source."
  (when mac-ime--saved-input-source
    (mac-ime-set-input-source mac-ime--saved-input-source)
    (setq mac-ime--saved-input-source nil))
  (remove-hook 'pre-command-hook #'mac-ime--restore-input-source))

(defun mac-ime-deactivate-ime-on-prefix (keycode modifiers)
  "Deactivate IME when a prefix key defined in `mac-ime-prefix-keys` is pressed.
This function is intended to be added to `mac-ime-functions`."
  (unless mac-ime--saved-input-source
    (cl-loop for (k . m) in mac-ime-prefix-keys
             if (and (= keycode k)
                     (= (logand modifiers m) m))
             return (let ((source (mac-ime--get-ime-off-input-source))
                          (current (mac-ime-get-input-source)))
                      (when (and source current (not (string= source current)))
                        (setq mac-ime--saved-input-source current)
                        (mac-ime-set-input-source source)
                        (add-hook 'pre-command-hook #'mac-ime--restore-input-source))))))

(defun mac-ime--load-module ()
  "Load the dynamic module if not already loaded."
  (unless (featurep 'mac-ime-module)
    (if (file-exists-p mac-ime-module-path)
        (module-load mac-ime-module-path)
      (message "mac-ime: Module not found at %s." mac-ime-module-path))))

(defun mac-ime-handler (keycode modifiers)
  "Internal handler called by the C module.
Calls functions in `mac-ime-functions`."
  (run-hook-with-args 'mac-ime-functions keycode modifiers)
  (mac-ime--check-input-source-change))
  

(defvar mac-ime-last-on-input-source nil
  "The last used input source ID that contains 'inputmethod'.")

(defvar mac-ime-last-off-input-source nil
  "The last used input source ID that contains 'keylayout'.")

(defvar mac-ime--current-input-source nil
  "Cache of the current input source ID.")

(defun mac-ime--check-input-source-change ()
  "Check if input source has changed and update `mac-ime-last-on-input-source` and `mac-ime-last-off-input-source`.
Only input sources containing 'inputmethod' are saved to on-source, and 'keylayout' to off-source."
  (let ((current (mac-ime-get-input-source)))
    (when (and current
               mac-ime--current-input-source
               (not (string= current mac-ime--current-input-source)))
      (cond
       ((string-match-p "inputmethod" mac-ime--current-input-source)
        (setq mac-ime-last-on-input-source mac-ime--current-input-source))
       ((string-match-p "keylayout" mac-ime--current-input-source)
        (setq mac-ime-last-off-input-source mac-ime--current-input-source))))
    (setq mac-ime--current-input-source current)))

(defun mac-ime-poll ()
  "Poll the C module for events."
  (when (featurep 'mac-ime-module)
    (mac-ime-internal-poll #'mac-ime-handler)))



;;;###autoload
(defun mac-ime-enable ()
  "Enable the global key monitor."
  (interactive)
  (mac-ime--load-module)
  (when (featurep 'mac-ime-module)
    (mac-ime-internal-start)
    (unless mac-ime-timer
      (setq mac-ime-timer (run-with-timer 0 mac-ime-poll-interval #'mac-ime-poll))
      (add-hook 'mac-ime-functions #'mac-ime-deactivate-ime-on-prefix)
      (dolist (func mac-ime-auto-deactivate-functions)
        (mac-ime-auto-deactivate func))
      (message "mac-ime enabled."))))

;;;###autoload
(defun mac-ime-disable ()
  "Disable the global key monitor."
  (interactive)
  (when mac-ime-timer
    (cancel-timer mac-ime-timer)
    (setq mac-ime-timer nil))
  (when (featurep 'mac-ime-module)
    (remove-hook 'mac-ime-functions #'mac-ime-deactivate-ime-on-prefix)
    (mac-ime-internal-stop)
    (dolist (func mac-ime-auto-deactivate-functions)
      (advice-remove func #'mac-ime--auto-deactivate-advice))
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
  (mac-ime--load-module)
  (when (featurep 'mac-ime-module)
    (mac-ime-internal-set-input-source source-id)))

;;;###autoload
(defun mac-ime-get-input-source-list ()
  "Get a list of all selectable input source IDs."
  (mac-ime--load-module)
  (when (featurep 'mac-ime-module)
    (mac-ime-internal-get-input-source-list)))

(defun mac-ime--auto-deactivate-advice (orig-fun &rest args)
  "Advice to deactivate IME before ORIG-FUN and restore it afterwards."
  (let ((saved-source (mac-ime-get-input-source))
        (off-source (mac-ime--get-ime-off-input-source)))
    (if (and off-source saved-source (not (string= off-source saved-source)))
        (progn
          (mac-ime-set-input-source off-source)
          (unwind-protect
              (apply orig-fun args)
            (mac-ime-set-input-source saved-source)))
      (apply orig-fun args))))

;;;###autoload
(defun mac-ime-auto-deactivate (func)
  "Add advice to FUNC to deactivate IME during its execution.
The IME state is restored after FUNC completes."
  (advice-add func :around #'mac-ime--auto-deactivate-advice))

;;;###autoload
(defun mac-ime-activate-ime ()
  "Activate the IME input source.
Uses `mac-ime--get-ime-on-input-source` to determine the input source."
  (interactive)
  (let ((source (mac-ime--get-ime-on-input-source)))
    (when source
      (mac-ime-set-input-source source))))

;;;###autoload
(defun mac-ime-deactivate-ime ()
  "Deactivate the IME input source.
Uses `mac-ime--get-ime-off-input-source` to determine the input source."
  (interactive)
  (let ((source (mac-ime--get-ime-off-input-source)))
    (when source
      (mac-ime-set-input-source source))))
      
(provide 'mac-ime)
;;; mac-ime.el ends here
