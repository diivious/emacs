;; -*-Emacs-Lisp-*-
;;
;; emacs white space highlighting init file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; hilight useless trailing whitespace tabs (\t) and such as:
;;

(setq whitespace-style '(face tabs spaces trailing lines-tail))

(font-lock-add-keywords
 nil
 '((" " . 'highlight) ; Highlight spaces
   ("\t" . 'highlight))) ; Highlight tabs


(global-whitespace-mode 1)


(message "emacs-wspace loaded")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; .emacs-wspace.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; eof ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
