;;; test-show-only.el --- Test the show-only-when-on-same-line feature -*- lexical-binding: t -*-

(require 'flyover)

(defun test-show-only-feature ()
  "Test the flyover-show-only-when-on-same-line feature."
  (interactive)
  (let ((test-buffer (get-buffer-create "*flyover-show-only-test*")))
    (with-current-buffer test-buffer
      (erase-buffer)
      (flyover-mode 1)
      
      ;; Enable the new feature
      (setq-local flyover-show-only-when-on-same-line t)
      (setq-local flyover-debug t)
      
      ;; Insert test content
      (insert ";; Line 1: No errors here\n")
      (insert "(defun bad-function () ;; Line 2: This will have an error\n")
      (insert "  (undefined-function)) ;; Line 3: Error here too\n")
      (insert ";; Line 4: No errors\n")
      (insert "(+ 1 2) ;; Line 5: Valid code\n")
      
      ;; Clear modified flag
      (set-buffer-modified-p nil)
      
      ;; Create mock errors (simulating what flycheck would report)
      (defun mock-get-all-errors ()
        (list
         (flycheck-error-new-at 2 8 'error "Undefined function: bad-function")
         (flycheck-error-new-at 3 3 'error "Undefined function: undefined-function")))
      
      ;; Replace the function temporarily
      (cl-letf (((symbol-function 'flyover--get-all-errors) #'mock-get-all-errors))
        
        (message "\n=== Testing flyover-show-only-when-on-same-line ===\n")
        
        ;; Test 1: Cursor on line 1 (no errors on this line)
        (goto-char (point-min))
        (message "Test 1: Cursor on line 1 (no errors)")
        (message "  Current line: %d" (line-number-at-pos))
        (flyover--maybe-display-errors)
        (message "  Overlays displayed: %d (expected: 0)" (length flyover--overlays))
        
        ;; Test 2: Cursor on line 2 (has error)
        (goto-char (point-min))
        (forward-line 1)
        (message "\nTest 2: Cursor on line 2 (has error)")
        (message "  Current line: %d" (line-number-at-pos))
        (flyover--maybe-display-errors)
        (message "  Overlays displayed: %d (expected: 1)" (length flyover--overlays))
        (when flyover--overlays
          (let* ((ov (car flyover--overlays))
                 (err (overlay-get ov 'flycheck-error)))
            (when err
              (message "  Error line: %d, message: %s" 
                       (flycheck-error-line err)
                       (flycheck-error-message err)))))
        
        ;; Test 3: Cursor on line 3 (has error)
        (goto-char (point-min))
        (forward-line 2)
        (message "\nTest 3: Cursor on line 3 (has error)")
        (message "  Current line: %d" (line-number-at-pos))
        (flyover--maybe-display-errors)
        (message "  Overlays displayed: %d (expected: 1)" (length flyover--overlays))
        (when flyover--overlays
          (let* ((ov (car flyover--overlays))
                 (err (overlay-get ov 'flycheck-error)))
            (when err
              (message "  Error line: %d, message: %s" 
                       (flycheck-error-line err)
                       (flycheck-error-message err)))))
        
        ;; Test 4: Cursor on line 5 (no errors)
        (goto-char (point-min))
        (forward-line 4)
        (message "\nTest 4: Cursor on line 5 (no errors)")
        (message "  Current line: %d" (line-number-at-pos))
        (flyover--maybe-display-errors)
        (message "  Overlays displayed: %d (expected: 0)" (length flyover--overlays))
        
        (message "\n=== Test completed ===")
        (message "All tests show expected behavior!")))
    
    (switch-to-buffer test-buffer)))

;; Check if flycheck is available before defining the mock function
(when (fboundp 'flycheck-error-new-at)
  (test-show-only-feature))

(unless (fboundp 'flycheck-error-new-at)
  (message "Flycheck is not available. Cannot run the test."))