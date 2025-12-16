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
(defconst ime-hook-mac-NSEventModifierFlagControl 262401 "Modifier flag for Control key.")

(defcustom ime-hook-mac-prefix-keys
  `((,ime-hook-mac-kVK_ANSI_X . ,ime-hook-mac-NSEventModifierFlagControl)
    (,ime-hook-mac-kVK_ANSI_C . ,ime-hook-mac-NSEventModifierFlagControl))
  "Alist of prefix keys that trigger IME deactivation.
Each element is a cons cell (KEYCODE . MODIFIERS).
If the keycode matches and the specified modifiers are set, IME is deactivated."
  :type '(alist :key-type integer :value-type integer)
  :group 'ime-hook-mac)

(defcustom ime-hook-mac-default-input-source nil
  "Input source ID to switch to when a prefix key is pressed.
If nil, the first input source containing 'keylayout' in its ID will be used."
  :type '(choice (const :tag "Auto-detect" nil)
                 (string :tag "Input Source ID"))
  :group 'ime-hook-mac)

(defvar ime-hook-mac--default-input-source-cache nil
  "Cache for the auto-detected default input source.")

(defun ime-hook-mac--get-default-input-source ()
  "Return the input source ID to use.
If `ime-hook-mac-default-input-source` is non-nil, return it.
Otherwise, find the first input source containing 'keylayout' and cache it."
  (or ime-hook-mac-default-input-source
      ime-hook-mac--default-input-source-cache
      (setq ime-hook-mac--default-input-source-cache
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
           return (let ((source (ime-hook-mac--get-default-input-source))
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

(defun ime-hook-mac-poll ()
  "Poll the C module for events."
  (when (featurep 'ime-hook-module)
    (ime-hook-internal-poll #'ime-hook-mac-handler)))



;;;###autoload
(defun ime-hook-mac-enable ()
  "Enable the global key monitor."
  (interactive)
  (ime-hook-mac--load-module)
  (when (featurep 'ime-hook-module)
    (ime-hook-internal-start)
    (unless ime-hook-mac-timer
      (setq ime-hook-mac-timer (run-with-timer 0 ime-hook-mac-poll-interval #'ime-hook-mac-poll))
      (message "ime-hook-mac enabled."))))

;;;###autoload
(defun ime-hook-mac-disable ()
  "Disable the global key monitor."
  (interactive)
  (when ime-hook-mac-timer
    (cancel-timer ime-hook-mac-timer)
    (setq ime-hook-mac-timer nil))
  (when (featurep 'ime-hook-module)
    (ime-hook-internal-stop)
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

(provide 'ime-hook-mac)
;;; ime-hook-mac.el ends here
