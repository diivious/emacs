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
  
(add-hook 'find-file-hook 'initialize-auto-fill-mode)

(message "emacs-21 loaded")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; .emacs-21.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; eof ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
