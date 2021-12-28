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
  "Kills trailing whitespace and tabifies."
  (interactive)
  (message "Compressing . . .")
  (beginning-of-buffer)
  (untabify-file)
  (perform-replace " +$" "" nil t nil)
  (tabify (point-min) (point-max))
  (message ""))

(message "emacs-cmn loaded")
