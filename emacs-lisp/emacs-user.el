;; -*-Emacs-Lisp-*-
;;
;; some of my own custom modifications and key bindings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   Note:  the following variables are set in the .emacs file.
;         Directories should NOT be terminated with a trailing "/".
;
;     my-home-directory  -- the full pathname of your home directory
;     my-elisp-directory -- the full pathname of a directory containing emacs
;                           "extras"
;     my-domain-name     -- the name of your mail domain
;     my-full-name       -- your full name
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "Installing custom command bindings")

(defvar gdb-many-windows)  ;; Declare the variable globally (optional)


(setq
; emacs with the '-q' flag read .emacs and evaluate each lisp sexp
; manually step-by-step...
 debug-on-error nil           ;debugging
 stack-trace-on-error nil     ;stack trace

 visible-bell t
 mouse-wheel-mode nil
 line-number-mode t

;Use the new gdb Emacs22 interface
  gdb-many-windows t
)

; show C function in lower bar
(which-function-mode)

; un-comment if you want tab bar
;;(tabbar-mode)

(message "Installing custom key bindings")
(put 'downcase-region 'disabled nil)
(put 'upcase-region   'disabled nil)
(put 'eval-expression 'disabled nil)

;;  "Strip trailing `^M' characters from the current output group.
;;This function could be on `comint-output-filter-functions' or bound to a key."
;;(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m nil t)

;;(setq truncate-partial-width-windows nil) 

(global-set-key "\C-x," 'compile)

(if (fboundp 'insert-box)
    (global-set-key "\C-cp" 'insert-box))

(if (fboundp 'backward-delete-char-untabify)
    (global-set-key "\C-?" 'backward-delete-char-untabify))

(global-set-key [C-up]    'scroll-down)
(global-set-key [C-down]  'scroll-up)

(global-set-key [C-left]  'backward-word)
(global-set-key [C-right] 'forward-word)

(global-set-key "\e\040"  'set-mark-command)
(global-set-key "\eg"     'goto-line)
(global-set-key "\er"     'query-replace)
(global-set-key "\er"     'query-replace)

(global-set-key [f1]      'set-mark-command)
(global-set-key [f2]      'other-window)
(global-set-key [f4]      'kill-line)
(global-set-key [f5]      'scroll-down)
(global-set-key [f6]      'scroll-up)

(global-set-key [f7]      'shrink-window)
(global-set-key [f8]      'enlarge-window)

(global-set-key [f9]      'save-buffer)
(global-set-key [f10]     'save-buffers-kill-emacs)

(global-set-key [C-home]  'beginning-of-buffer)
(global-set-key [C-end]   'end-of-buffer)

(message "emacs-user loaded")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; .emacs-user.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; eof ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
