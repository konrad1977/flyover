;;; test-compilation.el --- Test if flyover picks up compilation errors from flycheck

;; This test checks if flyover can display compilation errors
;; that are already in flycheck-current-errors

(defun flyover-debug-current-errors ()
  "Debug function to see what errors flyover is getting."
  (interactive)
  (let ((all-errors (flyover--get-all-errors)))
    (message "=== FLYOVER DEBUG ===")
    (message "Total errors found by flyover: %d" (length all-errors))
    (dolist (err all-errors)
      (message "  Flyover error: Line %s, Col %s, Level %s, Checker %s, Message: %s" 
               (flycheck-error-line err)
               (flycheck-error-column err)
               (flycheck-error-level err)
               (flycheck-error-checker err)
               (flycheck-error-message err))))
  
  (message "\n=== FLYCHECK CURRENT ERRORS ===")
  (message "Flycheck-current-errors: %d errors" (length flycheck-current-errors))
  (dolist (err flycheck-current-errors)
    (message "  Flycheck error: Line %s, Col %s, Level %s, Checker: %s, Message: %s"
             (flycheck-error-line err)
             (flycheck-error-column err)
             (flycheck-error-level err)
             (flycheck-error-checker err)
             (flycheck-error-message err)))
  
  ;; Check if errors are being filtered
  (message "\n=== FILTERING CHECK ===")
  (let ((filtered (flyover--filter-errors flycheck-current-errors)))
    (message "After filtering: %d errors (from %d)" 
             (length filtered) (length flycheck-current-errors))
    (when (/= (length filtered) (length flycheck-current-errors))
      (message "Some errors were filtered out!")
      (dolist (err flycheck-current-errors)
        (unless (memq err filtered)
          (message "  Filtered: Line %s, Level %s, Valid: %s"
                   (flycheck-error-line err)
                   (flycheck-error-level err)
                   (flyover--is-valid-error err)))))))

(defun flyover-test-compilation-support ()
  "Test if compilation errors in flycheck-current-errors are displayed by flyover."
  (interactive)
  (message "=== FLYOVER CONFIGURATION ===")
  (message "Flyover checkers enabled: %S" flyover-checkers)
  (message "Flyover mode: %s" (if flyover-mode "enabled" "disabled"))
  (message "Flyover levels: %S" flyover-levels)
  (message "Buffer file: %s" (buffer-file-name))
  
  (flyover-debug-current-errors)
  
  ;; Force a redisplay
  (message "\n=== FORCING REDISPLAY ===")
  (flyover--display-errors)
  (message "Overlays created: %d" (length flyover--overlays))
  
  ;; Check overlays
  (when flyover--overlays
    (message "Overlay details:")
    (dolist (ov flyover--overlays)
      (when (overlayp ov)
        (message "  Overlay at line %s" 
                 (line-number-at-pos (overlay-start ov)))))))

(defun flyover-test-single-error ()
  "Test creating an overlay for a single compilation error."
  (interactive)
  (if (not flycheck-current-errors)
      (message "No errors in flycheck-current-errors to test with")
    (let ((err (car flycheck-current-errors)))
      (message "Testing with error: Line %s, Level %s, Message: %s"
               (flycheck-error-line err)
               (flycheck-error-level err)
               (flycheck-error-message err))
      
      ;; Check if error is valid
      (message "Is valid error? %s" (flyover--is-valid-error err))
      
      ;; Try to get region
      (let ((region (flyover--get-error-region err)))
        (if region
            (progn
              (message "Got region: %S" region)
              ;; Try to create overlay
              (let ((ov (flyover--create-overlay 
                         region 
                         (flycheck-error-level err)
                         (flycheck-error-message err)
                         err)))
                (if ov
                    (message "Successfully created overlay!")
                  (message "Failed to create overlay"))))
          (message "Failed to get region for error"))))))

(provide 'test-compilation)
;;; test-compilation.el ends here