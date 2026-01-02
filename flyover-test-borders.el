;;; flyover-test-borders.el --- Tests for border styles -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for flyover-border-chars alist and flyover--get-border-chars function.

;;; Code:

(require 'ert)
(require 'flyover)

(ert-deftest flyover-test-border-chars-default-styles ()
  "Test that default border styles are defined in the alist."
  (should (assq 'pill flyover-border-chars))
  (should (assq 'arrow flyover-border-chars)))

(ert-deftest flyover-test-border-chars-structure ()
  "Test that border chars entries have correct structure (LEFT . RIGHT)."
  (dolist (entry flyover-border-chars)
    (let ((style (car entry))
          (chars (cdr entry)))
      (should (symbolp style))
      (should (consp chars))
      (should (stringp (car chars)))
      (should (stringp (cdr chars))))))

(ert-deftest flyover-test-get-border-chars-pill ()
  "Test that pill style returns correct border chars."
  (let ((flyover-border-style 'pill))
    (let ((result (flyover--get-border-chars)))
      (should (plist-get result :left))
      (should (plist-get result :right))
      (should (stringp (plist-get result :left)))
      (should (stringp (plist-get result :right))))))

(ert-deftest flyover-test-get-border-chars-arrow ()
  "Test that arrow style returns correct border chars."
  (let ((flyover-border-style 'arrow))
    (let ((result (flyover--get-border-chars)))
      (should (plist-get result :left))
      (should (plist-get result :right))
      (should (stringp (plist-get result :left)))
      (should (stringp (plist-get result :right))))))

(ert-deftest flyover-test-get-border-chars-none ()
  "Test that none style returns empty strings."
  (let ((flyover-border-style 'none))
    (let ((result (flyover--get-border-chars)))
      (should (string-empty-p (plist-get result :left)))
      (should (string-empty-p (plist-get result :right))))))

(ert-deftest flyover-test-get-border-chars-unknown ()
  "Test that unknown style returns empty strings."
  (let ((flyover-border-style 'nonexistent-style))
    (let ((result (flyover--get-border-chars)))
      (should (string-empty-p (plist-get result :left)))
      (should (string-empty-p (plist-get result :right))))))

(ert-deftest flyover-test-custom-border-style ()
  "Test that custom styles can be added and used."
  (let ((flyover-border-chars (copy-alist flyover-border-chars))
        (flyover-border-style 'custom-test))
    (add-to-list 'flyover-border-chars '(custom-test . ("[" . "]")))
    (let ((result (flyover--get-border-chars)))
      (should (equal (plist-get result :left) "["))
      (should (equal (plist-get result :right) "]")))))

(ert-deftest flyover-test-all-defined-styles-work ()
  "Test that all styles defined in alist return valid border chars."
  (dolist (entry flyover-border-chars)
    (let* ((style (car entry))
           (flyover-border-style style)
           (result (flyover--get-border-chars)))
      (should (plist-get result :left))
      (should (plist-get result :right)))))

(provide 'flyover-test-borders)
;;; flyover-test-borders.el ends here
