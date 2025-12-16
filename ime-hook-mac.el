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

(defun ime-hook-mac--load-module ()
  "Load the dynamic module if not already loaded."
  (unless (featurep 'ime-hook-module)
    (if (file-exists-p ime-hook-mac-module-path)
        (module-load ime-hook-mac-module-path)
      (message "ime-hook-mac: Module not found at %s. Please run `ime-hook-mac-build-module`." ime-hook-mac-module-path))))

(defun ime-hook-mac-handler (keycode modifiers)
  "Internal handler called by the C module.
Calls functions in `ime-hook-mac-functions`."
  (run-hook-with-args 'ime-hook-mac-functions keycode modifiers))

(defun ime-hook-mac-poll ()
  "Poll the C module for events."
  (when (featurep 'ime-hook-module)
    (ime-hook-internal-poll #'ime-hook-mac-handler)))

;;;###autoload
(defun ime-hook-mac-build-module ()
  "Build the dynamic module using make."
  (interactive)
  (let ((default-directory (file-name-directory (or load-file-name buffer-file-name))))
    (if (executable-find "make")
        (progn
          (message "Building ime-hook-mac module...")
          (start-process "ime-hook-build" "*ime-hook-build*" "make")
          (set-process-sentinel (get-process "ime-hook-build")
                                (lambda (p e)
                                  (when (equal e "finished\n")
                                    (message "ime-hook-mac module built successfully.")))))
      (error "Make not found"))))

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

(provide 'ime-hook-mac)
;;; ime-hook-mac.el ends here
