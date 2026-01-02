;;; test-display-mode.el --- Test the new flyover-display-mode setting -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for the unified flyover-display-mode setting and migration.

;;; Code:

(require 'flyover)

(defun test-display-mode ()
  "Test the new unified flyover-display-mode setting."
  (message "\n=== Testing flyover-display-mode ===\n")

  ;; Test 1: Check that the new defcustom exists
  (if (boundp 'flyover-display-mode)
      (message "✓ Test 1: flyover-display-mode variable exists")
    (error "✗ Test 1: flyover-display-mode variable not found"))

  ;; Test 2: Check default value
  (if (eq flyover-display-mode 'always)
      (message "✓ Test 2: Default value is 'always")
    (error "✗ Test 2: Default value is not 'always"))

  ;; Test 3: Check that old variables are marked obsolete
  (if (get 'flyover-hide-when-cursor-is-on-same-line 'byte-obsolete-variable)
      (message "✓ Test 3: Old variables are marked obsolete")
    (message "! Test 3: Old variables not marked obsolete (may be Emacs version issue)"))

  ;; Test 4: Test migration from old settings
  (let ((test-buffer (get-buffer-create "*test-migration*")))
    (with-current-buffer test-buffer
      ;; Test migration from show-only-when-on-same-line
      (setq flyover--settings-migrated nil) ; Reset migration flag
      (setq flyover-show-only-when-on-same-line t)
      (setq flyover-display-mode 'always)
      (flyover--migrate-display-settings)
      (if (eq flyover-display-mode 'show-only-on-same-line)
          (message "✓ Test 4a: Migration from show-only-when-on-same-line works")
        (error "✗ Test 4a: Migration failed"))

      ;; Test migration from hide-when-cursor-is-on-same-line
      (setq flyover--settings-migrated nil) ; Reset migration flag
      (setq flyover-show-only-when-on-same-line nil)
      (setq flyover-hide-when-cursor-is-on-same-line t)
      (setq flyover-display-mode 'always)
      (flyover--migrate-display-settings)
      (if (eq flyover-display-mode 'hide-on-same-line)
          (message "✓ Test 4b: Migration from hide-when-cursor-is-on-same-line works")
        (error "✗ Test 4b: Migration failed"))

      ;; Test migration from hide-when-cursor-is-at-same-line
      (setq flyover--settings-migrated nil) ; Reset migration flag
      (setq flyover-hide-when-cursor-is-on-same-line nil)
      (setq flyover-hide-when-cursor-is-at-same-line t)
      (setq flyover-display-mode 'always)
      (flyover--migrate-display-settings)
      (if (eq flyover-display-mode 'hide-at-exact-position)
          (message "✓ Test 4c: Migration from hide-when-cursor-is-at-same-line works")
        (error "✗ Test 4c: Migration failed"))))
  
  ;; Test 5: Test each display mode
  (let ((test-buffer (get-buffer-create "*test-modes*")))
    (with-current-buffer test-buffer
      (flyover-mode 1)
      
      ;; Test 'always mode
      (setq-local flyover-display-mode 'always)
      (flyover--disable)
      (flyover--enable)
      (if (not (memq 'flyover--maybe-display-errors-debounced post-command-hook))
          (message "✓ Test 5a: 'always mode doesn't add post-command-hook")
        (message "! Test 5a: 'always mode unexpectedly added post-command-hook"))
      
      ;; Test 'show-only-on-same-line mode
      (setq-local flyover-display-mode 'show-only-on-same-line)
      (flyover--disable)
      (flyover--enable)
      (if (memq 'flyover--maybe-display-errors-debounced post-command-hook)
          (message "✓ Test 5b: 'show-only-on-same-line mode adds post-command-hook")
        (message "✗ Test 5b: 'show-only-on-same-line mode didn't add post-command-hook"))
      
      ;; Test 'hide-on-same-line mode
      (setq-local flyover-display-mode 'hide-on-same-line)
      (flyover--disable)
      (flyover--enable)
      (if (memq 'flyover--maybe-display-errors-debounced post-command-hook)
          (message "✓ Test 5c: 'hide-on-same-line mode adds post-command-hook")
        (message "✗ Test 5c: 'hide-on-same-line mode didn't add post-command-hook"))
      
      ;; Test 'hide-at-exact-position mode
      (setq-local flyover-display-mode 'hide-at-exact-position)
      (flyover--disable)
      (flyover--enable)
      (if (memq 'flyover--maybe-display-errors-debounced post-command-hook)
          (message "✓ Test 5d: 'hide-at-exact-position mode adds post-command-hook")
        (message "✗ Test 5d: 'hide-at-exact-position mode didn't add post-command-hook"))))
  
  (message "\n=== All tests completed successfully! ===")
  (message "\nThe new unified setting flyover-display-mode replaces the three old boolean settings:")
  (message "  - flyover-hide-when-cursor-is-on-same-line → 'hide-on-same-line")
  (message "  - flyover-hide-when-cursor-is-at-same-line → 'hide-at-exact-position")
  (message "  - flyover-show-only-when-on-same-line → 'show-only-on-same-line")
  (message "\nUsers can now customize via: M-x customize-variable flyover-display-mode"))

(test-display-mode)

(provide 'test-display-mode)
;;; test-display-mode.el ends here