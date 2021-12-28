;; -*-Emacs-Lisp-*-
;;
;; emacs codeing style init file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun format-function ()
  "According to IOS rules, indent, remove whitespace and tabify function, from anywhere inside function, or on the function name"
  (interactive)
  (c-mark-function)
  (c-indent-line-or-region)
  (c-mark-function)
  (whitespace-cleanup-region (region-beginning) (region-end))
  (c-mark-function)
  (tabify (region-beginning) (region-end))
)

;;
;; sets c++ mode for all the following files
(setq auto-mode-alist
	(append '
		(("\\.[CH]$"  . c++-mode)
			("\\.[ch]pp$"  . c++-mode)
			("\\.cc$"  . c++-mode)
			("\\.c$"   . c++-mode)
			("\\.h$"   . c++-mode)
			("\\.reg$"  . c++-mode)
			("\\.c.gcov$" . c++-mode)
			("\\.my$"  . snmpv2-mode)
			("\\.mib$" . snmpv2-mode)
			("\\.lib$" . tcl-mode)
			("\\.suite$" . tcl-mode)
			("\\.pdl$" . quoted-lisp-mode))
		auto-mode-alist))

;;
;; c-mode customizations
;;
;;c-file-style "BSD"			; for cc-mode
c-mode-hook '(lambda () 
		(auto-fill-mode 1)
;;		(setq
;;		 c-argdecl-indent 4
;;		 c-auto-newline nil
;;		 c-brace-imaginary-offset 0
;;		 c-brace-offset 0
;;		 c-continued-statement-offset 4
;;		 c-indent-level 4
;;		 c-label-offset -4
;;		 comment-column 40
;;		 comment-start "/* "
;;		 comment-end " */"
;;		 comment-multi-line t
;;		 comment-start-skip "/\\* "
;;		 ;; cc-mode settings
;;		 c-basic-offset 4
;;		 )
		)
;;
;; my cc-mode settings
;;
(defvar diivious-c-style
  '((c-auto-newline                 . nil)
    (c-basic-offset                 . 4)
    (c-block-comments-indent-p      . nil)
    (c-comment-only-line-offset     . 0)
    (c-echo-syntactic-information-p . nil)
    (c-hanging-comment-ender-p      . t)
    (c-recognize-knr-p              . t) ; use nil if only have ANSI prototype
    (c-tab-always-indent            . nil)
    (comment-column                 . 40)
    (comment-end                    . " */")
    (comment-multi-line             . t)
    (comment-start                  . "/* ")
    (fill-column                    . 80)
    (vvb-mode                       . t)
    (c-hanging-comment-ender-p      . nil)
    (c-offsets-alist                . ((knr-argdecl-intro   . +)
                                       (case-label          . 0)
                                       (knr-argdecl         . 0)
                                       (label               . 0)
                                       (statement-case-open . +)
                                       (statement-cont      . +)
                                       (substatement-open   . 0))))
  "diivious c-style for cc-mode")

(add-hook 'c-mode-common-hook
	  '(lambda ()
	     (c-add-style "DIIVIOUS" diivious-c-style)
	     (c-set-style "DIIVIOUS")))

(message "emacs-style loaded")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; .emacs-style.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; eof ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
