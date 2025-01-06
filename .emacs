; -*-Emacs-Lisp-*-
;
; Donnie Savage's .emacs initialization file
;
(defvar *emacs-load-start* (current-time))

(defvar my-login-name (user-login-name)
  "*Non-nil simple name of this system.")

(if (null (posix-string-match "\\." (system-name)))
     (defvar my-domain-name "cisco.com"
       "*Non-nil info used in various places.")
  ;;else
    (defvar my-domain-name
      (substring (system-name) (1+ (posix-string-match "\\." (system-name))))
      "*Non-nil domain name of outgoing e-mail.")
)

(defvar my-host-name
  (substring (system-name) 0 (posix-string-match "\\." (system-name)))
  "*Non-nil simple name of this system.")

(if (string-match "linux" system-configuration)
    (defvar my-host-type "linux"
      "*Non-nil simple type of this system.")
  ;; else
    (defvar my-host-type "sun"
      "*Non-nil simple type of this system.")
)

(defvar my-old-load-path load-path
  "*Original load-path.")

;; * Dired is very slow.
;;
;; This could happen if invocation of the `df' program takes a long
;; time.  Possible reasons for this include:
;;
;;   - ClearCase mounted filesystems (VOBs) that sometimes make `df'
;;     response time extremely slow (dozens of seconds);
;;
;; To work around the problem, you could either (a) set the variable
;; `directory-free-space-program' to nil, and thus prevent Emacs from
;; invoking `df'; (b) use `df' from the GNU Fileutils package; or
;; (c) use CVS, which is Free Software, instead of ClearCase.
(setq directory-free-space-program nil)


;; * Redisplay using X11 is much slower than previous Emacs versions.
;;
;; We've noticed that certain X servers draw the text much slower when
;; scroll bars are on the left.  We don't know why this happens.  If this
;; happens to you, you can work around it by putting the scroll bars
;; on the right (as they were in Emacs 19).
(set-scroll-bar-mode 'nil)

;;(setq default-frame-alist '((vertical-scroll-bars . 'nil))
;;			    (menu-bar-lines . 'nil))

(if (file-exists-p "~/emacs-lisp")
  (defvar my-elisp-directory "~/emacs-lisp/"
    "*Non-nil info used in various places.")
)

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (and (fboundp 'setenv)
	 (not (string-match "bash" (getenv "SHELL")))
	 (file-executable-p "/bin/bash"))
    (setenv "SHELL" "/bin/bash"))

;;
;; start up the emacs pager service - delete this line if you dont
;; like cvs diffs to open in side emacs
;;(server-start)

;;; Define a function to make it easier to check which version we're
;;; running.
(defun takein-newer-init-file (el-file)
  "Check on the age of an emacs el file versus its byte-compiled version.
   byte-compile the file if necessary."
  (if (file-exists-p (concat my-elisp-directory el-file))
      (setq compile-el-file (concat my-elisp-directory el-file))
    ;; else
      (setq compile-el-file el-file))

  (setq compile-elc-file (concat compile-el-file "c"))

  (cond ((file-newer-than-file-p compile-el-file compile-elc-file)
	 (let ((mode-line-format
		(concat "Recompiling " compile-el-file " ..." )))
	   (message "Recompiling %s ..." compile-el-file)
	   (byte-compile-file compile-el-file)
	   (message "Done."))
	 ))
  (message "Loading %s ..." compile-elc-file)
  (load (expand-file-name compile-elc-file) nil t t)
)

(message ".emacs loading.")

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I turn these on and off a lot.... make it easy
(defvar load-color   t)		;; color for emacs
(defvar load-eigrp   t)		;; eigrp functions
(defvar load-gdb     t)		;; gdb functions
(defvar load-login   t)		;; per user settings
(defvar load-style   t)		;; my style
(defvar load-user    t)		;; default user setting
(defvar load-wspace  t)		;; color white space

(takein-newer-init-file "emacs-cmn.el")

;;determine if we should run with 19
(if (string-match "^19" emacs-version)
    (takein-newer-init-file "emacs-19.el")

  ;; else if
  ;;determine if we should run with 21 setting
  (if (string-match "^21" emacs-version)
      (takein-newer-init-file "emacs-21.el")

    ;; else if
    ;;determine if we should run with or 2x setting
    (takein-newer-init-file "emacs-2x.el")
  )
)

(if (not (null load-color))	(takein-newer-init-file "emacs-color.el"))	;; emacs color file
(if (not (null load-eigrp))	(takein-newer-init-file "emacs-eigrp.el"))	;; codeing eigrp file
(if (not (null load-style))	(takein-newer-init-file "emacs-style.el"))	;; codeing style file
(if (not (null load-wspace))	(takein-newer-init-file "emacs-wspace.el"))	;; emacs wspace file
(if (not (null load-gdb))	(takein-newer-init-file "emacs-gdb.el"))	;; emacs gdb file
(if (not (null load-user))	(takein-newer-init-file "emacs-user.el"))	;; emacs per user file

(message ".emacs load complete")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; .emacs.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; eof ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


