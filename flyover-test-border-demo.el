;;; flyover-test-border-demo.el --- Visual demo of all border styles -*- lexical-binding: t -*-

;;; Commentary:
;; Interactive demo showing all available border styles.
;; Run M-x flyover-border-demo to see all styles.

;;; Code:

(require 'flyover)

(defun flyover-border-demo ()
  "Display a buffer showing all available border styles."
  (interactive)
  (let ((buf (get-buffer-create "*Flyover Border Styles*")))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (insert "Flyover Border Styles Demo\n")
      (insert "==========================\n\n")
      (insert "Available styles in `flyover-border-chars`:\n\n")

      ;; Show each style
      (dolist (entry flyover-border-chars)
        (let* ((style (car entry))
               (chars (cdr entry))
               (left (car chars))
               (right (cdr chars))
               (sample-bg "#3a3a5a")
               (sample-fg "#ffffff"))
          (insert (format "  %-12s  " style))
          ;; Render a sample overlay
          (insert (propertize left 'face `(:foreground ,sample-bg)))
          (insert (propertize " Example error message " 'face `(:background ,sample-bg :foreground ,sample-fg)))
          (insert (propertize right 'face `(:foreground ,sample-bg)))
          (insert "\n")))

      (insert "\n")
      (insert "Style 'none' shows no borders.\n\n")
      (insert "Usage:\n")
      (insert "  (setq flyover-border-style 'pill)   ; or arrow, slant, etc.\n\n")
      (insert "Add custom styles:\n")
      (insert "  (add-to-list 'flyover-border-chars '(my-style . (\"[\" . \"]\")))\n")
      (insert "  (setq flyover-border-style 'my-style)\n")

      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

(defun flyover-border-demo-cycle ()
  "Cycle through border styles to preview them live."
  (interactive)
  (let* ((styles (mapcar #'car flyover-border-chars))
         (all-styles (cons 'none styles))
         (current flyover-border-style)
         (idx (or (cl-position current all-styles) -1))
         (next-idx (mod (1+ idx) (length all-styles)))
         (next-style (nth next-idx all-styles)))
    (setq flyover-border-style next-style)
    (flyover-refresh)
    (message "Border style: %s" next-style)))

(provide 'flyover-test-border-demo)
;;; flyover-test-border-demo.el ends here
