;;
;; color.....
;;
;;
;; Load the font-lock package.
(require 'font-lock)

;; Maximum colors
(setq font-lock-maximum-decoration t)

;; Turn on font-lock in all modes that support it
(global-font-lock-mode t)
(transient-mark-mode t)

;;
;; color.....
;;
(add-hook 'find-file-hook 'turn-on-font-lock)
(add-hook 'c-mode-hook 'turn-on-font-lock)
(add-hook 'dired-mode-hook 'turn-on-font-lock)
(add-hook 'html-mode-hook 'turn-on-font-lock)

(message "emacs-color loaded")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; .emacs-color.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; eof ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
