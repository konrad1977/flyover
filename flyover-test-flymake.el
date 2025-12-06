;;; flymake-test.el --- Test file for Flymake diagnostics -*- lexical-binding: t -*-

;;; code:

(require 'flymake)
(require 'flyover)

(defun flymake-test-backend (report-fn &rest _args)
  "Test Flymake backend that creates sample diagnostics."
  (let* ((buffer (current-buffer))
         (diagnostics (list
                       ;; Create error diagnostic
                       (flymake-make-diagnostic buffer
                                                (+ (point-min) 1)
                                                (+ (point-min) 9)
                                                :error
                                                "This is a test error message")
                       
                       ;; Create warning diagnostic
                       (flymake-make-diagnostic buffer
                                                (+ (point-min) 10)
                                                (+ (point-min) 25)
                                                :warning
                                                "This is a test warning message")
                       
                       ;; Create info diagnostic
                       (flymake-make-diagnostic buffer
                                                (+ (point-min) 30)
                                                (+ (point-min) 35)
                                                :note
                                                "This is a test info message"))))
    
    ;; Use the report-fn callback to report diagnostics
    (funcall report-fn diagnostics)))

(defun flymake-test-run ()
  "Run Flymake test diagnostics."
  (interactive)
  (let ((buf (get-buffer-create "*flymake-test*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "\nTest buffer for Flymake diagnostics\n\n\n")
      
      ;; Add the backend to flymake-diagnostic-functions
      (add-hook 'flymake-diagnostic-functions 'flymake-test-backend nil t)
      
      ;; Start Flymake and enable overlay mode
      (flymake-mode 1)
      (flyover-mode 1)
      
      ;; Trigger a check
      (flymake-start))
    (switch-to-buffer buf)))

(defun flymake-test-run-all ()
  "Run comprehensive Flymake test with different error types."
  (interactive)
  (let ((buf (get-buffer-create "*flymake-test*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "Flymake Test Cases:\n\n")
      (insert "1. Error case: This line has an error\n")
      (insert "2. Warning case: This line has a warning\n") 
      (insert "3. Info case: This line has an info message\n")
      (insert "4. Multi-line case: This error spans\nmultiple lines\n")
      
      ;; Add the backend
      (add-hook 'flymake-diagnostic-functions 'flymake-test-backend nil t)
      
      ;; Enable both modes
      (flymake-mode 1)
      (flyover-mode 1)
      
      ;; Trigger check
      (flymake-start))
    (switch-to-buffer buf)))

(flymake-test-run)

(defun flymake-test-verify-overlays ()
  "Verify that overlays are present in test buffer."
  (interactive)
  (let ((overlays (overlays-in (point-min) (point-max)))
        (found nil))
    (dolist (ov overlays)
      (when (overlay-get ov 'flyover)
        (setq found t)
        (message "Found flyover overlay at %d-%d: %s"
                 (overlay-start ov) (overlay-end ov)
                 (overlay-get ov 'after-string))))
    (unless found
      (message "No flyover overlays found!"))))

(provide 'flymake-test)
;;; flymake-test.el ends here
