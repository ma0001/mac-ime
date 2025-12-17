;;; ime-hook-mac.el --- NSEvent hook for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Masami
;; Author: Masami
;; Version: 0.1.0
;; Keywords: mac, input, ime
;; Package-Requires: ((emacs "25.1"))
;; URL: https://github.com/masami/ime-hook-mac

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package provides a way to hook into macOS global key events
;; using a dynamic module. It is intended to be used for IME integration.

;;; Code:

(require 'cl-lib)

(defvar ime-hook-mac-module-file "ime-hook-module.so"
  "Name of the dynamic module file.")

(defvar ime-hook-mac-module-path
  (expand-file-name ime-hook-mac-module-file (file-name-directory (or load-file-name buffer-file-name)))
  "Full path to the dynamic module.")

(defvar ime-hook-mac-timer nil
  "Timer object for polling events.")

(defvar ime-hook-mac-poll-interval 0.05
  "Interval in seconds to poll for events.")

(defcustom ime-hook-mac-functions nil
  "List of functions to call when a key event occurs.
Each function is called with two arguments: (keycode modifiers)."
  :type 'hook
  :group 'ime-hook-mac)

(defconst ime-hook-mac-kVK_ANSI_X 7 "Virtual key code for 'x'.")
(defconst ime-hook-mac-kVK_ANSI_C 8 "Virtual key code for 'c'.")
(defconst ime-hook-mac-kVK_ANSI_H 4 "Virtual key code for 'h'.")
(defconst ime-hook-mac-kVK_ANSI_G 5 "Virtual key code for 'g'.")
(defconst ime-hook-mac-kVK_Escape 53 "Virtual key code for 'Escape'.")
(defconst ime-hook-mac-NSEventModifierFlagControl 262401 "Modifier flag for Control key.")
(defconst ime-hook-mac-NSEventModifierFlagCmd 1048840 "Modifier flag for Cmd key.")

(defcustom ime-hook-mac-prefix-keys
  `((,ime-hook-mac-kVK_ANSI_X . ,ime-hook-mac-NSEventModifierFlagControl)
    (,ime-hook-mac-kVK_ANSI_C . ,ime-hook-mac-NSEventModifierFlagControl)
    (,ime-hook-mac-kVK_ANSI_H . ,ime-hook-mac-NSEventModifierFlagControl)
    (,ime-hook-mac-kVK_ANSI_G . ,ime-hook-mac-NSEventModifierFlagCmd)
    (,ime-hook-mac-kVK_Escape . 0))
  "Alist of prefix keys that trigger IME deactivation.
Each element is a cons cell (KEYCODE . MODIFIERS).
If the keycode matches and the specified modifiers are set, IME is deactivated."
  :type '(alist :key-type integer :value-type integer)
  :group 'ime-hook-mac)

(defcustom ime-hook-mac-ime-off-input-source nil
  "Input source ID to switch to when a prefix key is pressed (to turn off IME).
If nil, the first input source containing 'keylayout' in its ID will be used."
  :type '(choice (const :tag "Auto-detect" nil)
                 (string :tag "Input Source ID"))
  :group 'ime-hook-mac)

(defcustom ime-hook-mac-auto-deactivate-functions '(universal-argument read-string read-char read-from-minibuffer y-or-n-p yes-or-no-p map-y-or-n-p)
  "List of functions to automatically deactivate IME during execution."
  :type '(repeat function)
  :group 'ime-hook-mac)

(defvar ime-hook-mac--ime-off-input-source-cache nil
  "Cache for the auto-detected IME off input source.")

(defun ime-hook-mac--get-ime-off-input-source ()
  "Return the input source ID to use to turn off IME.
If `ime-hook-mac-ime-off-input-source` is non-nil, return it.
Otherwise, find the first input source containing 'keylayout' and cache it."
  (or ime-hook-mac-ime-off-input-source
      ime-hook-mac--ime-off-input-source-cache
      (setq ime-hook-mac--ime-off-input-source-cache
            (cl-loop for source in (ime-hook-mac-get-input-source-list)
                     if (string-match-p "keylayout" source)
                     return source))))

(defvar ime-hook-mac--saved-input-source nil
  "Saved input source ID to restore.")

(defun ime-hook-mac--restore-input-source ()
  "Restore the saved input source."
  (when ime-hook-mac--saved-input-source
    (ime-hook-mac-set-input-source ime-hook-mac--saved-input-source)
    (setq ime-hook-mac--saved-input-source nil))
  (remove-hook 'pre-command-hook #'ime-hook-mac--restore-input-source))

(defun ime-hook-mac-deactivate-ime-on-prefix (keycode modifiers)
  "Deactivate IME when a prefix key defined in `ime-hook-mac-prefix-keys` is pressed.
This function is intended to be added to `ime-hook-mac-functions`."
  (cl-loop for (k . m) in ime-hook-mac-prefix-keys
           if (and (= keycode k)
                   (= (logand modifiers m) m))
           return (let ((source (ime-hook-mac--get-ime-off-input-source))
                        (current (ime-hook-mac-get-input-source)))
                    (when (and source current (not (string= source current)))
                      (setq ime-hook-mac--saved-input-source current)
                      (ime-hook-mac-set-input-source source)
                      (add-hook 'pre-command-hook #'ime-hook-mac--restore-input-source)))))

(defun ime-hook-mac--load-module ()
  "Load the dynamic module if not already loaded."
  (unless (featurep 'ime-hook-module)
    (if (file-exists-p ime-hook-mac-module-path)
        (module-load ime-hook-mac-module-path)
      (message "ime-hook-mac: Module not found at %s." ime-hook-mac-module-path))))

(defun ime-hook-mac-handler (keycode modifiers)
  "Internal handler called by the C module.
Calls functions in `ime-hook-mac-functions`."
  (run-hook-with-args 'ime-hook-mac-functions keycode modifiers))

(defvar ime-hook-mac-last-input-source nil
  "The last used input source ID that contains 'inputmethod'.")

(defvar ime-hook-mac--current-input-source nil
  "Cache of the current input source ID.")

(defun ime-hook-mac--check-input-source-change ()
  "Check if input source has changed and update `ime-hook-mac-last-input-source`.
Only input sources containing 'inputmethod' are saved."
  (let ((current (ime-hook-mac-get-input-source)))
    (when (and current
               ime-hook-mac--current-input-source
               (not (string= current ime-hook-mac--current-input-source)))
      (when (string-match-p "inputmethod" ime-hook-mac--current-input-source)
        (setq ime-hook-mac-last-input-source ime-hook-mac--current-input-source)))
    (setq ime-hook-mac--current-input-source current)))

(defun ime-hook-mac-poll ()
  "Poll the C module for events."
  (when (featurep 'ime-hook-module)
    (ime-hook-internal-poll #'ime-hook-mac-handler)
    (ime-hook-mac--check-input-source-change)))



;;;###autoload
(defun ime-hook-mac-enable ()
  "Enable the global key monitor."
  (interactive)
  (ime-hook-mac--load-module)
  (when (featurep 'ime-hook-module)
    (ime-hook-internal-start)
    (unless ime-hook-mac-timer
      (setq ime-hook-mac-timer (run-with-timer 0 ime-hook-mac-poll-interval #'ime-hook-mac-poll))
      (add-hook 'ime-hook-mac-functions #'ime-hook-mac-deactivate-ime-on-prefix)
      (dolist (func ime-hook-mac-auto-deactivate-functions)
        (ime-hook-mac-auto-deactivate func))
      (message "ime-hook-mac enabled."))))

;;;###autoload
(defun ime-hook-mac-disable ()
  "Disable the global key monitor."
  (interactive)
  (when ime-hook-mac-timer
    (cancel-timer ime-hook-mac-timer)
    (setq ime-hook-mac-timer nil))
  (when (featurep 'ime-hook-module)
    (remove-hook 'ime-hook-mac-functions #'ime-hook-mac-deactivate-ime-on-prefix)
    (ime-hook-internal-stop)
    (dolist (func ime-hook-mac-auto-deactivate-functions)
      (advice-remove func #'ime-hook-mac--auto-deactivate-advice))
    (message "ime-hook-mac disabled.")))

;;;###autoload
(defun ime-hook-mac-get-input-source ()
  "Get the current input source ID."
  (ime-hook-mac--load-module)
  (when (featurep 'ime-hook-module)
    (ime-hook-internal-get-input-source)))

;;;###autoload
(defun ime-hook-mac-set-input-source (source-id)
  "Set the current input source to SOURCE-ID."
  (ime-hook-mac--load-module)
  (when (featurep 'ime-hook-module)
    (ime-hook-internal-set-input-source source-id)))

;;;###autoload
(defun ime-hook-mac-get-input-source-list ()
  "Get a list of all selectable input source IDs."
  (ime-hook-mac--load-module)
  (when (featurep 'ime-hook-module)
    (ime-hook-internal-get-input-source-list)))

(defun ime-hook-mac--auto-deactivate-advice (orig-fun &rest args)
  "Advice to deactivate IME before ORIG-FUN and restore it afterwards."
  (let ((saved-source (ime-hook-mac-get-input-source))
        (off-source (ime-hook-mac--get-ime-off-input-source)))
    (if (and off-source saved-source (not (string= off-source saved-source)))
        (progn
          (ime-hook-mac-set-input-source off-source)
          (unwind-protect
              (apply orig-fun args)
            (ime-hook-mac-set-input-source saved-source)))
      (apply orig-fun args))))

;;;###autoload
(defun ime-hook-mac-auto-deactivate (func)
  "Add advice to FUNC to deactivate IME during its execution.
The IME state is restored after FUNC completes."
  (advice-add func :around #'ime-hook-mac--auto-deactivate-advice))

;;;###autoload
(defun ime-hook-mac-activate-ime ()
  "Activate the last used IME input source.
If `ime-hook-mac-last-input-source` is nil, use the first available
input source containing 'inputmethod'."
  (interactive)
  (let ((source (or ime-hook-mac-last-input-source
                    (cl-loop for s in (ime-hook-mac-get-input-source-list)
                             if (string-match-p "inputmethod" s)
                             return s))))
    (when source
      (ime-hook-mac-set-input-source source))))
      
(provide 'ime-hook-mac)
;;; ime-hook-mac.el ends here
