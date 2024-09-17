;; -*-Emacs-Lisp-*-
;;
;; emacs white space highlighting init file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; hilight useless trailing whitespace tabs (\t) and such as:
;;

(defun highlight-wspace ()
  "Enable whitespace highlighting for tabs, spaces, and trailing whitespace."
  ;; Enable highlighting for tabs
  (when (not highlight-tabs-p)
    (toggle-tabs-font-lock))
  ;; Enable highlighting for spaces
  (when (not highlight-spaces-p)
    (toggle-space-font-lock))
  ;; Enable highlighting for trailing whitespace
  (when (not highlight-trailing-whitespace-p)
    (toggle-trailing-whitespace-font-lock)))

;; Example: Use `prog-mode-hook` to enable it in programming modes
(add-hook 'prog-mode-hook 'highlight-wspace)

;; Use `text-mode-hook` to enable it in text-related modes
(add-hook 'text-mode-hook 'highlight-wspace)

;; If you want to enable it globally for all buffers
(add-hook 'after-change-major-mode-hook 'highlight-wspace)

(message "emacs-wspace loaded")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; .emacs-wspace.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; eof ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
