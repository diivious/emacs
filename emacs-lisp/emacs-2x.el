;; -*-Emacs-Lisp-*-
;;
;; emacs 2x init file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun initialize-auto-fill-mode ()
  "Turns on auto fill mode if the flag auto-fill-default is non-nil"
  (if (and (not noninteractive)
	   auto-fill-default)
      (auto-fill-mode 1)))

(add-hook 'find-file-hook 'initialize-auto-fill-mode)

(defun dvs-compile-start (command &optional mode buf-name highlight-regexp)
  (compilation-start command mode
                     (lambda (mode-name) (concat "*" buf-name "*"))
                     highlight-regexp))

(message "emacs-2x loaded")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; .emacs-2x.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; eof ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
