(require 'ert)
(require 'mac-ime)

(ert-deftest mac-ime-load-test ()
  "Test that the module loads and functions are defined."
  (mac-ime--load-module)
  (should (featurep 'mac-ime))
  (should (featurep 'mac-ime-module))
  (should (fboundp 'mac-ime-internal-start))
  (should (fboundp 'mac-ime-internal-stop))
  (should (fboundp 'mac-ime-internal-poll)))

(ert-deftest mac-ime-start-stop-test ()
  "Test starting and stopping the monitor."
  (mac-ime--load-module)
  (should (equal (mac-ime-internal-start) t))
  (should (equal (mac-ime-internal-stop) t)))

(ert-deftest mac-ime-input-source-test ()
  "Test getting and setting input source."
  (mac-ime--load-module)
  (should (fboundp 'mac-ime-internal-get-input-source))
  (should (fboundp 'mac-ime-internal-set-input-source))
  (should (fboundp 'mac-ime-internal-get-input-source-list))
  
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
