;; -*-Emacs-Lisp-*-
;;
;; emacs 19.x init file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

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

 ;;
 ;; auto-show-mode enable automatic horizontal scrolling, when lines
 ;; are truncated so that the user need not use C-x< and C-x> to
 ;; manually move the view.
 ;;
(auto-show-mode t)

;;
;; alow eval-expression (default binding on ESC ESC)
;;	Evaluate EXPRESSION and print value in minibuffer.
;;	Value is also consed on to front of variable `values'.
;;
(put 'eval-expression 'disabled nil)
(load "rsz-mini" t t nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; custom lisp functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun toggle-truncate-lines ()
  "Toggle the value of truncate-lines"
  (interactive)
  (setq truncate-lines (not truncate-lines))
  (scroll-up 0))

(message "emacs-19 loaded")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; .emacs-19.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; eof ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
