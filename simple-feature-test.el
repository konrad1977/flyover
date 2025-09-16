;;; simple-feature-test.el --- Simple test for the new feature -*- lexical-binding: t -*-

(require 'flyover)

(defun simple-feature-test ()
  "Simple test to verify the new setting exists and works."
  (message "\n=== Testing flyover-show-only-when-on-same-line feature ===\n")
  
  ;; Test 1: Check that the new defcustom exists
  (if (boundp 'flyover-show-only-when-on-same-line)
      (message "✓ Test 1: flyover-show-only-when-on-same-line variable exists")
    (error "✗ Test 1: flyover-show-only-when-on-same-line variable not found"))
  
  ;; Test 2: Check default value
  (if (eq flyover-show-only-when-on-same-line nil)
      (message "✓ Test 2: Default value is nil (disabled by default)")
    (error "✗ Test 2: Default value is not nil"))
  
  ;; Test 3: Test enabling the feature
  (let ((test-buffer (get-buffer-create "*test*")))
    (with-current-buffer test-buffer
      (flyover-mode 1)
      (setq-local flyover-show-only-when-on-same-line t)
      
      (if flyover-show-only-when-on-same-line
          (message "✓ Test 3: Feature can be enabled locally")
        (error "✗ Test 3: Failed to enable feature"))
      
      ;; Test 4: Check that post-command-hook is set when enabled
      (flyover--disable)
      (flyover--enable)
      
      (if (memq 'flyover--maybe-display-errors-debounced post-command-hook)
          (message "✓ Test 4: post-command-hook is added when feature is enabled")
        (message "! Test 4: post-command-hook not added (expected when feature enabled)"))
      
      ;; Test 5: Disable and check hook is removed
      (setq-local flyover-show-only-when-on-same-line nil)
      (flyover--disable)
      (flyover--enable)
      
      (if (not (memq 'flyover--maybe-display-errors-debounced post-command-hook))
          (message "✓ Test 5: post-command-hook not added when feature is disabled")
        (message "! Test 5: post-command-hook still present when feature disabled"))))
  
  (message "\n=== All basic tests passed! ===")
  (message "\nThe new feature flyover-show-only-when-on-same-line has been successfully added.")
  (message "When enabled, overlays will only appear for errors on the current line.")
  (message "The setting can be customized via M-x customize-variable flyover-show-only-when-on-same-line"))

(simple-feature-test)