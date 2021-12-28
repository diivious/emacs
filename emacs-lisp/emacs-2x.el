;; -*-Emacs-Lisp-*-
;;
;; emacs 22.x init file
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
  
(if (not (memq 'initialize-auto-fill-mode find-file-hook))
    (setq find-file-hook (cons 'initialize-auto-fill-mode find-file-hook)))

(defun dvs-compile-start (command &optional mode buf-name highlight-regexp)
  (if (fboundp 'compilation-start)    ; Emacs 22
      (compilation-start command mode
			 #'(lambda (mode-name) (concat "*" buf-name "*")) highlight-regexp)
    ; else
    (compile-internal command
		      "No more hits" buf-name nil highlight-regexp)))

(message "emacs-22 loaded")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; .emacs-22.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; eof ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
