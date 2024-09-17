;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compress-buffer.el --- kill trailing whitespace and tabs.
;; 
;; Author          : Mark D. Baushke
;; Created On      : Tue Mar  7 15:37:55 1989
;; Last Modified By: Mark D. Baushke
;; Last Modified On: Tue Mar  7 15:38:23 1989
;; Update Count    : 1
;; Status          : OK.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun compress-buffer ()
  "Remove trailing whitespace, untabify, and then tabify the current buffer."
  (interactive)
  (message "Compressing . . .")
  ;; Move to the beginning of the buffer
  (goto-char (point-min))

  ;; Untabify the entire buffer
  (untabify (point-min) (point-max))

  ;; Remove trailing whitespace
  (while (re-search-forward "[ \t]+$" nil t)
    (replace-match "" nil nil))

  ;; Tabify the entire buffer
  (tabify (point-min) (point-max))

  (message "Compression done."))

(message "emacs-cmn loaded")
