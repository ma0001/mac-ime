;;; mac-ime-integration-test.el --- Integration tests for mac-ime -*- lexical-binding: t; -*-

(require 'ert)
(require 'mac-ime)

(ert-deftest mac-ime-integration-load-test ()
  "Test loading the actual dynamic module."
  ;; Ensure the module file exists
  (let ((mac-ime-module-path (expand-file-name "mac-ime-module.so" default-directory)))
    (should (file-exists-p mac-ime-module-path))
    
    ;; Load the module
    (mac-ime--load-module)
    
    ;; Check if features and functions are present
    (should (featurep 'mac-ime-module))
    (should (fboundp 'mac-ime-internal-start))
    (should (fboundp 'mac-ime-internal-stop))
    (should (fboundp 'mac-ime-internal-poll))
    (should (fboundp 'mac-ime-internal-get-input-source))
    (should (fboundp 'mac-ime-internal-set-input-source))
    (should (fboundp 'mac-ime-internal-get-input-source-list))))

(ert-deftest mac-ime-integration-api-test ()
  "Test calling actual module functions."
  (mac-ime--load-module)
  
  ;; Test getting input source
  (let ((source (mac-ime-internal-get-input-source)))
    (should (stringp source))
    (message "Actual Input Source: %s" source))
  
  ;; Test getting input source list
  (let ((sources (mac-ime-internal-get-input-source-list)))
    (should (listp sources))
    (should (cl-every #'stringp sources))
    (message "Available Sources: %s" sources))
  
  ;; Test start/stop (basic check)
  (should (equal (mac-ime-internal-start) t))
  (should (equal (mac-ime-internal-stop) t)))

(provide 'mac-ime-integration-test)
