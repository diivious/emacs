;; -*-Emacs-Lisp-*-
;;
;; emacs white space highlighting init file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; hilight useless trailing whitespace tabs (\t) and such as:
;;
;;(require 'font-lock)
;;(make-face (quote font-lock-blanks-face))
;;(set-face-background (quote font-lock-blanks-face) "Gray75")
;;(add-hook 'font-lock-mode-hook
;;          '(lambda()
;;             (setq font-lock-keywords
;;                   (append font-lock-keywords
;;                           '(("[ \t]+$" (0 'font-lock-blanks-face t)))))))
(defun highlight-wspace ()
  "Enable whitespace highlighting for tabs, spaces, and trailing whitespace."
  (require 'whitespace) ;; Ensure that whitespace-mode is available
  (setq-local whitespace-style '(face tabs spaces trailing))
  (whitespace-mode 1))

(add-hook 'font-lock-mode-hook 'highlight-wspace)

(message "emacs-wspace loaded")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; .emacs-wspace.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; eof ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
