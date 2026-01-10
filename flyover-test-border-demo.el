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

      ;; Get theme colors for each level
      (let* ((error-colors (flyover--get-face-colors 'error))
             (warning-colors (flyover--get-face-colors 'warning))
             (info-colors (flyover--get-face-colors 'info))
             (levels `((error ,flyover-error-icon ,(car error-colors) ,(cdr error-colors))
                       (warning ,flyover-warning-icon ,(car warning-colors) ,(cdr warning-colors))
                       (info ,flyover-info-icon ,(car info-colors) ,(cdr info-colors)))))

        ;; Show each style with all three levels
        (dolist (entry flyover-border-chars)
          (let* ((style (car entry))
                 (chars (cdr entry))
                 (left (car chars))
                 (right (cdr chars)))
            (insert (format "  %s\n" style))
            (dolist (level levels)
              (let* ((level-name (nth 0 level))
                     (icon (nth 1 level))
                     (fg-color (nth 2 level))
                     (bg-color (nth 3 level))
                     (icon-bg (flyover--tint-color fg-color 'darker 50))
                     (tinted-fg (flyover--tint-color fg-color 'lighter 50)))
                (insert "    ")
                ;; Left border
                (insert (propertize left 'face `(:foreground ,icon-bg)))
                ;; Icon
                (insert (propertize (format " %s " icon) 'face `(:foreground ,tinted-fg :background ,icon-bg)))
                ;; Message
                (insert (propertize (format " %s message " level-name) 'face `(:foreground ,tinted-fg :background ,bg-color)))
                ;; Right border
                (insert (propertize right 'face `(:foreground ,bg-color)))
                (insert "\n")))
            (insert "\n"))))

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
