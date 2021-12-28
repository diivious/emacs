;; -*-Emacs-Lisp-*-
;;
;; emacs 21.x init file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; auto-fill
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun initialize-auto-fill-mode ()
  "Turns on auto fill mode if the flag auto-fill-default is non-nil"
  (if (and (not noninteractive)
	   auto-fill-default)
      (auto-fill-mode 1)))
  
(if (not (memq 'initialize-auto-fill-mode find-file-hooks))
    (setq find-file-hooks (cons 'initialize-auto-fill-mode find-file-hooks)))

(message "emacs-21 loaded")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; .emacs-21.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; eof ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
