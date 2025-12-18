(require 'ert)
(require 'mac-ime)

(ert-deftest ime-hook-load-test ()
  "Test that the module loads and functions are defined."
  (mac-ime--load-module)
  (should (featurep 'mac-ime))
  (should (featurep 'ime-hook-module))
  (should (fboundp 'ime-hook-internal-start))
  (should (fboundp 'ime-hook-internal-stop))
  (should (fboundp 'ime-hook-internal-poll)))

(ert-deftest ime-hook-start-stop-test ()
  "Test starting and stopping the monitor."
  (mac-ime--load-module)
  (should (equal (ime-hook-internal-start) t))
  (should (equal (ime-hook-internal-stop) t)))

(ert-deftest ime-hook-input-source-test ()
  "Test getting and setting input source."
  (mac-ime--load-module)
  (should (fboundp 'ime-hook-internal-get-input-source))
  (should (fboundp 'ime-hook-internal-set-input-source))
  (should (fboundp 'ime-hook-internal-get-input-source-list))
  
  (let ((current (mac-ime-get-input-source))
        (source-list (mac-ime-get-input-source-list)))
    (message "Current Input Source: %s" current)
    (message "Input Source List: %s" source-list)
    
    (should (or (stringp current) (null current)))
    (should (listp source-list))
    (when source-list
      (should (cl-every #'stringp source-list)))
    
    (when current
      ;; Try setting it to the same value
      (should (mac-ime-set-input-source current)))))
