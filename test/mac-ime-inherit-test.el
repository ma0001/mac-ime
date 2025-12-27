;;; mac-ime-inherit-test.el --- Tests for IME inheritance logic -*- lexical-binding: t; -*-

(require 'ert)
(require 'mac-ime)
(require 'mac-ime-mock (expand-file-name "mac-ime-mock.el" (file-name-directory load-file-name)))

;; Override module loading to use mock
(defun mac-ime--load-module ()
  (require 'mac-ime-mock)
  (provide 'mac-ime-module))

(defun mac-ime-test-reset ()
  (mac-ime-mock-reset)
  (setq mac-ime-timer nil)
  (setq current-input-method nil)
  (setq mac-ime-last-on-input-source nil)
  (setq mac-ime-last-off-input-source nil)
  (setq mac-ime--current-input-source nil)
  (setq mac-ime--saved-input-source nil)
  (setq mac-ime--ignore-input-source-change nil)
  (setq mac-ime--sync-paused nil)
  (setq mac-ime--expected-input-source nil))

(ert-deftest mac-ime-inherit-input-method-test ()
  "Test that IME state is inherited when configured."
  (mac-ime-test-reset)
  
  ;; Setup: IME is ON
  (setq mac-ime-debug-level 2)
  (mac-ime-internal-set-input-source "com.apple.inputmethod.Kotoeri.RomajiTyping")
  (setq current-input-method mac-ime-input-method)
  (setq mac-ime-last-off-input-source "com.apple.keylayout.US")
  
  (let ((inner-source nil)
        (mac-ime-auto-deactivate-functions mac-ime-auto-deactivate-functions))
    
    (defun mac-ime-test-inherit-func (arg1 inherit)
      (setq inner-source (mac-ime-internal-get-input-source)))
    
    ;; Register function with inherit index 1
    (add-to-list 'mac-ime-auto-deactivate-functions '(mac-ime-test-inherit-func . 1))
    (mac-ime-auto-deactivate '(mac-ime-test-inherit-func . 1))
    
    ;; Case 1: inherit is nil -> Should deactivate
    (mac-ime-test-inherit-func "dummy" nil)
    (should (equal inner-source "com.apple.keylayout.US"))
    (should (equal (mac-ime-internal-get-input-source) "com.apple.inputmethod.Kotoeri.RomajiTyping"))
    
    ;; Case 2: inherit is t -> Should keep IME ON
    (mac-ime-test-inherit-func "dummy" t)
    (should (equal inner-source "com.apple.inputmethod.Kotoeri.RomajiTyping"))
    (should (equal (mac-ime-internal-get-input-source) "com.apple.inputmethod.Kotoeri.RomajiTyping"))
    
    ;; Cleanup
    (advice-remove 'mac-ime-test-inherit-func (intern "mac-ime--auto-deactivate-mac-ime-test-inherit-func"))))
