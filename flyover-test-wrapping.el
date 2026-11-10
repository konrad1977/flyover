;;; test-wrapping.el --- Simple tests for message wrapping functionality -*- lexical-binding: t -*-

;; Load only the functions we need for testing
(defun flycheck-overlay-replace-curly-quotes (text)
  "Replace curly quotes with straight quotes in TEXT."
  (replace-regexp-in-string "[""]" "\""
    (replace-regexp-in-string "['']" "'" text)))

(defvar flycheck-overlay-wrap-messages t
  "Whether to wrap long error messages across multiple lines.")

(defvar flycheck-overlay-max-line-length 80
  "Maximum length of each line when wrapping messages.")

(defun flycheck-overlay--wrap-message (msg max-length)
  "Wrap MSG to multiple lines with each line no longer than MAX-LENGTH.
Returns a list of strings, each representing a line."
  (let ((lines '())
        (source-lines (split-string msg "\n" nil)))
    (dolist (source-line source-lines)
      (if (not flycheck-overlay-wrap-messages)
          (push source-line lines)
        (let ((words (split-string source-line " " t))
              (current-line ""))
          (dolist (word words)
            (let ((potential-line (if (string-empty-p current-line)
                                      word
                                    (concat current-line " " word))))
              (if (<= (length potential-line) max-length)
                  (setq current-line potential-line)
                ;; Current word would make line too long
                (when (not (string-empty-p current-line))
                  (push current-line lines))
                (setq current-line word))))
          ;; Add remaining words of line if it's not empty and reset current-line
          (when (not (string-empty-p current-line))
            (push current-line lines)))))
    (nreverse lines)))

;; Simple test runner
(defun test-equal (expected actual test-name)
  "Simple test function."
  (if (equal expected actual)
      (message "✓ PASS: %s" test-name)
    (message "✗ FAIL: %s\n  Expected: %S\n  Actual: %S" test-name expected actual)))

;; Test cases
(let ((flycheck-overlay-wrap-messages t)
      (flycheck-overlay-max-line-length 80))
  (test-equal '("Short message")
              (flycheck-overlay--wrap-message "Short message" 80)
              "Short message not wrapped"))

(let ((flycheck-overlay-wrap-messages t)
      (flycheck-overlay-max-line-length 20))
  (test-equal '("This is a very long" "error message that" "should be wrapped")
              (flycheck-overlay--wrap-message "This is a very long error message that should be wrapped" 20)
              "Long message wrapped correctly"))

(let ((flycheck-overlay-wrap-messages nil)
      (flycheck-overlay-max-line-length 20))
  (test-equal '("This is a very long error message that should not be wrapped")
              (flycheck-overlay--wrap-message "This is a very long error message that should not be wrapped" 20)
              "Wrapping disabled"))

(let ((flycheck-overlay-wrap-messages t)
      (flycheck-overlay-max-line-length 10))
  (test-equal '("supercalifragilisticexpialidocious")
              (flycheck-overlay--wrap-message "supercalifragilisticexpialidocious" 10)
              "Single long word"))

(let ((flycheck-overlay-wrap-messages t)
      (flycheck-overlay-max-line-length 80))
  (test-equal '()
              (flycheck-overlay--wrap-message "" 80)
              "Empty message"))

(let ((flycheck-overlay-wrap-messages t)
      (flycheck-overlay-max-line-length 20))
  (test-equal '("Exactly twenty chars")
              (flycheck-overlay--wrap-message "Exactly twenty chars" 20)
              "Message exactly at max length"))

(let ((flycheck-overlay-wrap-messages t)
      (flycheck-overlay-max-line-length 20))
  (test-equal '("Word with multiple" "spaces should wrap")
              (flycheck-overlay--wrap-message "Word    with    multiple    spaces    should    wrap" 20)
              "Multiple spaces handled"))

(let ((flycheck-overlay-wrap-messages t)
      (flycheck-overlay-max-line-length 20))
  (test-equal '("Newlines" "should" "be" "wrapped")
              (flycheck-overlay--wrap-message "Newlines\nshould\nbe\nwrapped" 20)
              "Newlines handled"))

(message "\nAll tests completed!")

;;; test-wrapping.el ends here