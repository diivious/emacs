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
(require 'show-wspace)
(add-hook 'font-lock-mode-hook 'highlight-tabs)
(add-hook 'font-lock-mode-hook 'highlight-spaces)
(add-hook 'font-lock-mode-hook 'highlight-trailing-whitespace)

(message "emacs-wspace loaded")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; .emacs-wspace.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; eof ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
