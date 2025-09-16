;;; flyover-test.el --- Test utilities for flyover -*- lexical-binding: t -*-

;; Only load if flyover is available

;;; Code:

(condition-case nil
    (require 'flyover)
  (error
   (message "Warning: flyover not available. Some tests may not work.")))

(require 'ert)

;; Try to load flycheck for testing
(condition-case nil
    (require 'flycheck)
  (error
   (message "Warning: flycheck not available. Some tests may not work.")))

(defun flyover-test-insert-errors ()
  "Insert sample errors into current buffer for testing.
Returns a list of created errors for verification."
  (let ((errors '()))
    ;; Clear buffer
    (erase-buffer)
    
    ;; Insert some code with errors
    (insert "Line 0: This line has an error\n")
    (insert "Line 1: This line has an error\n")
    (insert "Line 2: This line has a warning\n")
    (insert "Line 3: This line has an info message\n")
    (insert "Line 4: Multiple errors on one line\n")
    (insert "Line 5:\n")
    (insert "Line 6:\n")
    (insert "Line 7: This line has no errors or warnings \n")
    (insert "Line 8:\n")

    ;; Create error objects
    (push (flycheck-error-new-at 0 1 'error "Error at line 0 column 1") errors)
    (push (flycheck-error-new-at 2 2 'error "Error at line 2 column 2") errors)
    (push (flycheck-error-new-at 3 3 'warning "Warning at line 3 column 3") errors)
    (push (flycheck-error-new-at 4 4 'info "Info at line 4 and column 4") errors)
    (push (flycheck-error-new-at 5 5 'error "Error at line 5 column 5") errors)
    (push (flycheck-error-new-at 5 20 'warning "Warning at line 5 column 20") errors)
    (push (flycheck-error-new-at 9 5 'warning "Extra blank line detected at line 8 and column 5") errors)

    ;; Display the errors
    (flyover--display-errors errors)
    
    errors))

(defun flyover-test-buffer ()
  "Create a new buffer with test errors and return it."
  (let ((test-buffer (get-buffer-create "*flyover-test*")))
    (with-current-buffer test-buffer
      (erase-buffer)
      (flyover-mode 1)
      (flyover-test-insert-errors))
    test-buffer))

;; Tests for message wrapping functionality

(ert-deftest flyover-test-wrap-message-short ()
  "Test that short messages are not wrapped."
  (let ((flyover-wrap-messages t)
        (flyover-max-line-length 80))
    (should (equal (flyover--wrap-message "Short message" 80)
                   '("Short message")))))

(ert-deftest flyover-test-wrap-message-long ()
  "Test that long messages are wrapped correctly."
  (let ((flyover-wrap-messages t)
        (flyover-max-line-length 20))
    (should (equal (flyover--wrap-message "This is a very long error message that should be wrapped" 20)
                   '("This is a very long" "error message that" "should be wrapped")))))

(ert-deftest flyover-test-wrap-message-disabled ()
  "Test that wrapping is disabled when flyover-wrap-messages is nil."
  (let ((flyover-wrap-messages nil)
        (flyover-max-line-length 20))
    (should (equal (flyover--wrap-message "This is a very long error message that should not be wrapped" 20)
                   '("This is a very long error message that should not be wrapped")))))

(ert-deftest flyover-test-wrap-message-single-word ()
  "Test wrapping with a single long word."
  (let ((flyover-wrap-messages t)
        (flyover-max-line-length 10))
    (should (equal (flyover--wrap-message "supercalifragilisticexpialidocious" 10)
                   '("supercalifragilisticexpialidocious")))))

(ert-deftest flyover-test-wrap-message-empty ()
  "Test wrapping with empty message."
  (let ((flyover-wrap-messages t)
        (flyover-max-line-length 80))
    (should (equal (flyover--wrap-message "" 80)
                   '()))))

(ert-deftest flyover-test-wrap-message-exact-length ()
  "Test wrapping with message exactly at max length."
  (let ((flyover-wrap-messages t)
        (flyover-max-line-length 20))
    (should (equal (flyover--wrap-message "Exactly twenty chars" 20)
                   '("Exactly twenty chars")))))

(ert-deftest flyover-test-show-only-when-cursor-on-same-line ()
  "Test that flyover-show-only-when-on-same-line shows overlays only on current line."
  (skip-unless (fboundp 'flycheck-error-new-at))
  (with-temp-buffer
    (flyover-mode 1)
    (setq-local flyover-show-only-when-on-same-line t)
    
    ;; Insert test content
    (insert "Line 1: No errors\n")
    (insert "Line 2: Has error\n")
    (insert "Line 3: Has warning\n")
    (insert "Line 4: No errors\n")
    
    ;; Clear the modified flag
    (set-buffer-modified-p nil)
    
    ;; Create sample errors
    (let ((errors (list
                   (flycheck-error-new-at 2 5 'error "Error on line 2")
                   (flycheck-error-new-at 3 10 'warning "Warning on line 3"))))
      
      ;; Mock flyover--get-all-errors to return our test errors
      (cl-letf (((symbol-function 'flyover--get-all-errors) (lambda () errors)))
        
        ;; Test with cursor on line 1 (no errors)
        (goto-char (point-min))
        (flyover--maybe-display-errors)
        (should (= (length flyover--overlays) 0))
        
        ;; Test with cursor on line 2 (has error)
        (goto-char (point-min))
        (forward-line 1)
        (flyover--maybe-display-errors)
        (should (= (length flyover--overlays) 1))
        (let ((ov (car flyover--overlays)))
          (should (overlayp ov))
          (let ((err (overlay-get ov 'flycheck-error)))
            (should (flycheck-error-p err))
            (should (eq (flycheck-error-level err) 'error))
            (should (= (flycheck-error-line err) 2))))
        
        ;; Test with cursor on line 3 (has warning)
        (goto-char (point-min))
        (forward-line 2)
        (flyover--maybe-display-errors)
        (should (= (length flyover--overlays) 1))
        (let ((ov (car flyover--overlays)))
          (should (overlayp ov))
          (let ((err (overlay-get ov 'flycheck-error)))
            (should (flycheck-error-p err))
            (should (eq (flycheck-error-level err) 'warning))
            (should (= (flycheck-error-line err) 3))))
        
        ;; Test with cursor on line 4 (no errors)
        (goto-char (point-min))
        (forward-line 3)
        (flyover--maybe-display-errors)
        (should (= (length flyover--overlays) 0))))))

(ert-deftest flyover-test-wrap-message-multiple-spaces ()
  "Test wrapping with multiple spaces."
  (let ((flyover-wrap-messages t)
        (flyover-max-line-length 20))
    (should (equal (flyover--wrap-message "Word    with    multiple    spaces    should    wrap" 20)
                   '("Word with multiple" "spaces should wrap")))))

(defun flyover-test-multiline-errors ()
  "Insert sample errors with long messages for multiline testing.
Returns a list of created errors for verification."
  (let ((errors '()))
    ;; Clear buffer
    (erase-buffer)

    (setq-local flyover-levels '(error info))
    
    ;; Insert some code with errors
    (insert "function calculateComplexValue(param) {\n")
    (insert "    return param * undefined_variable;\n")
    (insert "}\n")
    (insert "\n")
    (insert "const result = calculateComplexValue(42);\n")

    ;; Create error objects with long messages
    (push (flycheck-error-new-at 2 20 'error 
                                 "ReferenceError: undefined_variable is not defined. This variable has not been declared in the current scope. Please check if you meant to use a different variable name or declare this variable before using it.")
          errors)
    (push (flycheck-error-new-at 5 7 'warning
                                 "Variable 'result' is assigned a value but never used. Consider removing this variable or using it somewhere in your code to avoid this warning.")
          errors)
    (push (flycheck-error-new-at 1 1 'info
                                 "Consider adding JSDoc documentation for this function. This will help other developers understand the purpose, parameters, and return value of this function.")
          errors)

    ;; Display the errors
    (flyover--display-errors errors)
    
    errors))

(defun flyover-test-multiline-buffer ()
  "Create a new buffer with multiline test errors and return it."
  (let ((test-buffer (get-buffer-create "*flyover-multiline-test*")))
    (with-current-buffer test-buffer
      (erase-buffer)
      (flyover-mode 1)
      ;; Enable wrapping for testing
      (setq-local flyover-wrap-messages t)
      (setq-local flyover-max-line-length 50)
      (flyover-test-multiline-errors))
    test-buffer))

(ert-deftest flyover-test-multiline-display ()
  "Test that multiline overlays are created properly."
  (with-temp-buffer
    (let ((flyover-wrap-messages t)
          (flyover-max-line-length 30))
      ;; Setup buffer content
      (insert "some code here\n")
      (insert "more code\n")
      
      ;; Create a long error message
      (let* ((long-msg "This is a very long error message that should definitely be wrapped across multiple lines")
             (error (flycheck-error-new-at 2 5 'error long-msg))
             (region (cons (line-beginning-position 2) (line-end-position 2))))
        
        ;; Test that the message is wrapped
        (let ((wrapped (flyover--wrap-message long-msg 30)))
          (should (> (length wrapped) 1))
          (should (cl-every (lambda (line) (<= (length line) 30)) wrapped)))))))

(ert-deftest flyover-test-line-position-offset ()
  "Test that line position offset works correctly."
  (with-temp-buffer
    (let ((flyover-line-position-offset 2))
      ;; Setup buffer content
      (insert "line 1\n")
      (insert "line 2 with error\n")
      (insert "line 3\n")
      (insert "line 4\n")
      
      ;; Create error at line 2
      (let* ((error (flycheck-error-new-at 2 5 'error "Test error"))
             (region (progn
                       (goto-char (point-min))
                       (forward-line 1)
                       (cons (line-beginning-position) (line-end-position))))
             (overlay (flyover--create-overlay region 'error "Test error" error)))
        
        (should (overlayp overlay))
        ;; The overlay should start at line 2 but extend to line 4 (2 + offset)
        (goto-char (overlay-start overlay))
        (should (= (line-number-at-pos) 2))))))

;; Test runner function
(defun flyover-run-tests ()
  "Run all flyover tests."
  (interactive)
  (ert-run-tests-interactively "flyover-test-"))

;; Example usage:
(switch-to-buffer (flyover-test-buffer))

;; (switch-to-buffer (flyover-test-multiline-buffer))

(provide 'flyover-test)
;;; flyover-test.el ends here
