(require 'ert)
(require 'ime-hook-mac)

(ert-deftest ime-hook-load-test ()
  "Test that the module loads and functions are defined."
  (ime-hook-mac--load-module)
  (should (featurep 'ime-hook-mac))
  (should (featurep 'ime-hook-module))
  (should (fboundp 'ime-hook-internal-start))
  (should (fboundp 'ime-hook-internal-stop))
  (should (fboundp 'ime-hook-internal-poll)))

(ert-deftest ime-hook-start-stop-test ()
  "Test starting and stopping the monitor."
  (ime-hook-mac--load-module)
  (should (equal (ime-hook-internal-start) t))
  (should (equal (ime-hook-internal-stop) t)))

(ert-deftest ime-hook-input-source-test ()
  "Test getting and setting input source."
  (ime-hook-mac--load-module)
  (should (fboundp 'ime-hook-internal-get-input-source))
  (should (fboundp 'ime-hook-internal-set-input-source))
  
  (let ((current (ime-hook-mac-get-input-source)))
    (message "Current Input Source: %s" current)
    (should (or (stringp current) (null current)))
    
    (when current
      ;; Try setting it to the same value
      (should (ime-hook-mac-set-input-source current)))))
