;; -*-Emacs-Lisp-*-
;;
;; some of my own custom modifications and key bindings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; grep the eigrp component
;;
(defvar grep-cmd "grep --color -nH --null ")

(defun grep-frr (args)
  "Grep EIGRP with user-specified args, and collect output in a buffer"

 (interactive (list (read-string "grep frr (with args): "
				 (my-symbol-around-point))))

 (dvs-compile-start (concat grep-cmd "-R " args " ~/devel/frr/*")
		    'grep-mode grep-cmd grep-regexp-alist)
 )

(defun grep-eigrp (args)
  "Grep EIGRP with user-specified args, and collect output in a buffer"

 (interactive (list (read-string "grep eigrp (with args): "
				 (my-symbol-around-point))))

 (dvs-compile-start (concat grep-cmd args " *.[ch]")
		    'grep-mode grep-cmd grep-regexp-alist)
 )

;;
(message "emacs-eigrp loaded")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; .emacs-eigrp.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; eof ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
