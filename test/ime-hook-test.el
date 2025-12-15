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
