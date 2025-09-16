;;; debug-test.el --- Debug the show-only-on-same-line feature

(require 'flyover)

(defun debug-show-only-test ()
  "Debug test for show-only-on-same-line feature."
  (interactive)
  (let ((test-buffer (get-buffer-create "*debug-show-only*")))
    (with-current-buffer test-buffer
      (erase-buffer)
      
      ;; Enable flyover mode with debugging
      (flyover-mode 1)
      (setq-local flyover-display-mode 'show-only-on-same-line)
      (setq-local flyover-debug t)
      
      ;; Insert some test content
      (insert "Line 1: No errors\n")
      (insert "Line 2: Has errors\n") 
      (insert "Line 3: Has errors\n")
      (insert "Line 4: No errors\n")
      
      (message "Debug test setup complete.")
      (message "Set flyover-display-mode to 'show-only-on-same-line")
      (message "Set flyover-debug to t")
      (message "Now move cursor around and watch the debug messages."))
    
    (switch-to-buffer test-buffer)
    (goto-char (point-min))))

;; Run the debug test
(debug-show-only-test)