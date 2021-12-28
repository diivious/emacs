;; -*-Emacs-Lisp-*-
;;
;; emacs 19/21/22.x init file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Personal information
;;
;;(require 'cl)
(require 'compile)

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

(defvar my-full-name (user-full-name)
  "*Customized version of (user-full-name).")

(defvar  my-home-directory (getenv "ALTHOME")
  "*Non-nil info used in various places.")

; toolbar
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (fboundp 'tooltip-mode)
    (tooltip-mode -1))

; scroll bar
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

; menu bar
(if (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))

;;
;; Control variable auto-fill-default, default to "on". Turn "off" if
;; desired by setting it to nil.
;;
(defvar auto-fill-default t
  "*Non-nil says by default do auto-filling of every file-visiting buffer.")

;;
;;
(setq
 ;;
 ;; load path info
 ;;
 load-path (append (list
		    my-elisp-directory)
		   my-old-load-path)

 ;;
 ;; clearcase support has lots of symlnks, try to ues one true name
 ;;
 find-file-visit-truename t

 ;;
 ;; region highlighting (fun idea from bcole)
 ;;
 transient-mark-mode t
 mark-even-if-inactive t

 ;;
 ;; configuration preferences
 ;; 
 next-line-add-newlines nil
 require-final-newline t
 insert-directory-program "/bin/ls"	; better than "ls"

 ;;
 ;; defaults to be overridden
 ;;
 auto-save-interval 100			; save frequently (default is 300)

 ;;
 ;; show all the holidays that would appear in a complete Christian
 ;; calendar.
 all-christian-calendar-holidays t
 )

 ;;
 ;; Default the perldb command to be used to "perl5" instead of "perl"
 ;;
(defvar perldb-command-name "perl5")

;;
;; allow narrow-to-region (default binding on C-x n n)
;;	narrow-to-region: an interactive built-in function.
;;	
;;	Restrict editing in this buffer to the current region.
;;	The rest of the text becomes temporarily invisible and untouchable
;;	but is not deleted; if you save the buffer in a file, the invisible
;;	text is included in the file.  C-x n w makes all visible again.
;;	See also `save-restriction'.
;;	
;;	When calling from a program, pass two arguments; positions (integers
;;	or markers) bounding the text that should remain visible.
;;	
;;	arguments: (b e)
(put 'narrow-to-region 'disabled nil)

;;
;; allow downcase-region (default binding on C-x C-l)
;;	Convert the region to lower case.  In programs, wants two arguments.
;;	These arguments specify the starting and ending character numbers of
;;	the region to operate on.  When used as a command, the text between
;;	point and the mark is operated on.
;;
;;	arguments: (b e)
;;(put 'downcase-region 'disabled nil)

;;
;; allow upcase-region (default binding on C-x C-u)
;;	Convert the region to upper case.  In programs, wants two arguments.
;;	These arguments specify the starting and ending character numbers of
;;	the region to operate on.  When used as a command, the text between
;;	point and the mark is operated on.
;;	See also `capitalize-region'.
;;	
;;	arguments: (b e)
;;(put 'upcase-region 'disabled nil)

(defvar display-time-string nil)
(defvar display-time-timer nil)
(defvar display-time-process nil)

(display-time)
(defun un-display-time ()
  "Turn off display-time functionality."
  (interactive)
  (if display-time-timer
    (progn
      (cancel-timer display-time-timer)
      (setq display-time-timer nil
	    display-time-string nil)
      (message "display-time-timer canceled.")
      (sit-for 0)
      )))

(defun iconify-frame-new (&optional frame)
  "Turn off display-time if it is running.
Make the frame FRAME into an icon.
If omitted, FRAME defaults to the currently selected frame."
  (interactive)
  (un-display-time)
  (iconify-frame frame))

(if (fboundp 'raise-frame)
    (defun diivious-raise-frame () "Raise the current frame."
      (interactive)
      (raise-frame (selected-frame))))

(if (fboundp 'lower-frame)
    (defun diivious-lower-frame () "Raise the current frame."
      (interactive)
      (lower-frame (selected-frame))))

(global-set-key "\C-cw" 'compare-windows)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Emacs lisp debugging
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; When using bleeding edge list, it is important to run with the
;; debugger on at all times. The fdb package helps ignore errors that
;; are not critical to the task at hand.
;;(require 'fdb)
(setq debug-on-error nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; play with the shell
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar tshell-input-autoexpand nil)
(defvar tshell-prompt-pattern nil)

(setq
 shell-prompt-pattern "^\\(\\([0-9]*:.*@.*[%#$]\\|[0-9]+ ->\\|(gdb)\\)[ \t]+\\)+"
 tshell-prompt-pattern "^\\(\\([0-9]*:.*@.*[%#$]\\|[0-9]+ ->\\|(gdb)\\)[ \t]+\\)+"
 )
(if (not (load "tshell" t t nil))
    (require 'shell))

 ;;
 ;; shell mode settings
 ;;
(add-hook
 'shell-mode-hook
 '(lambda () (define-key shell-mode-map "\C-a" 'beginning-of-line)))
(add-hook
 'tshell-mode-hook
 '(lambda () (define-key tshell-mode-map "\C-a" 'beginning-of-line)))
(setq tshell-input-autoexpand nil)

(defun multi-shell (&optional shellnum)
  "Get a shell and number it. The command also takes an optional argument
N which if non-NIL will be used to find the buffer with N as the
number. (e.g., '*s' N '*')."
  (interactive)

  (if shellnum
      ;; First, does the requested buffer exist.
      ;; note we can't use format here as shellnum is not guarenteed
      ;; to be an integer.
      (let* ((shellname (concat "*s"  shellnum "*"))
	     (shellbuffer (get-buffer shellname)))
	(if shellbuffer
	    (switch-to-buffer shellbuffer) ;ok switch to the buffer
	  (error "no such buffer as %s" shellname)))

;; else
    ;; ok, we are to create a new shell buffer.
    ;; note that if *shell* exists, it will be renamed to *sn*
    ;; where n is an integer from 1 up.
    (if (fboundp 'tshell)
	(tshell)
      (shell))

    ;; Find an open slot in the form *sn* where n starts at 1
    (let ((shellnumber 1)
	  (shellname)
	  )
      (while (get-buffer (setq shellname (concat "*s" (int-to-string shellnumber) "*")))
	(setq shellnumber (1+ shellnumber)))
      (rename-buffer shellname))
))

(if (fboundp 'multi-shell)
    (progn
      (global-set-key "\C-xc" 'multi-shell)
      (global-set-key "\C-cy" 'multi-shell))
  (global-set-key "\C-cy" 'shell)
  (global-set-key "\C-xc" 'shell))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; title bar for emacs windows
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun title-set-title ()
  "Set title to current buffer's name \\[buffer-file-name]
or to \\[buffer-name] if it has no file"
  (let* ((buf-file (or (buffer-file-name (current-buffer))
		       (concat (expand-file-name default-directory)
			       " (" (buffer-name (current-buffer)) ")")))
;;	 (name (format "%s!%s:%s%s"
;;			my-host-name
;;			(user-login-name)
	 (name (format "Emacs:%s:%s"
			(system-name)
			buf-file)))
    (modify-frame-parameters (selected-frame)
			     (list (cons 'name name)))))

(setq post-command-hook (cons 'title-set-title post-command-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; custom lisp functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun delete-line ()
  "Move to the beginning of the current line and kill one line."
  (interactive)
  (beginning-of-line)
  (kill-line 1))

(defun top ()
  "Move to the beginning of the current buffer."
  (interactive)
  (move-to-window-line 0))

(defun center ()
  "Move to the center of the current buffer."
  (interactive)
  (move-to-window-line nil))

(defun bottom ()
  "Move to the bottom of the current buffer."
  (interactive)
  (move-to-window-line -1))

(defun tabify-and-trim ()
  "Used to tabify the current buffer and remove trailing whitespace."
  (interactive)
  (save-excursion
    (tabify (point-min) (point-max))
    (goto-char (point-min))
    (while (re-search-forward " +$" nil t)
      (replace-match "" nil nil))))

(defun untabify-and-trim ()
  "Used to untabify the current buffer and remove trailing whitespace."
  (interactive)
  (save-excursion
    (untabify (point-min) (point-max))
    (goto-char (point-min))
    (while (re-search-forward " +$" nil t)
      (replace-match "" nil nil))))

(defun untabify-file ()
  "Used to untabify the entire file."
  (interactive)
  (untabify 1 (+ 1 (buffer-size))))

(defun toggle-overwrite-mode ()
  "Toggle overwrite mode."
  (interactive)
  (overwrite-mode nil)
  ;; Indicate that the text of the mode-line has changed.
  (set-buffer-modified-p (buffer-modified-p))
  ;; Do redisplay right now, if no input pending
  (sit-for 0))

(defun insert-box (start end text)
  "Insert a text prefix at a column in all the lines in the region.
   Called from a program, takes three arguments, START, END, and TEXT.
   The column is taken from that of START.
   The rough inverse of this function is kill-rectangle."
  (interactive "r\nsText To Insert: ")
  (save-excursion
    (let (cc)
      ;; the point-marker stuff is needed to keep the edits from changing
      ;; where end is
      (goto-char end)
      (setq end (point-marker))
      (goto-char start)
      (setq cc  (current-column))
      (while (< (point) end) ;; modified 2/2/88
	;; I should here check for tab chars
	(insert text)
	(forward-line 1)
	(move-to-column cc))
      (move-marker end nil))))

(defun insert-suffix (start end text)
  "Insert a text prefix at the end in all the lines in the region.
   Called from a program, takes three arguments, START, END, and TEXT.
   The column is taken from that of START."
  (interactive "r\nsText To Insert: ")
  (save-excursion
    (let (cc)
      ;; the point-marker stuff is needed to keep the edits from changing
      ;; where end is
      (goto-char end)
      (setq end (point-marker))
      (goto-char start)
      (end-of-line)	
      (while (< (point) end);; modified 2/2/88
	;; I should here check for tab chars
	(insert text)
	(forward-line 1)
	(end-of-line)	
	)
      (move-marker end nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; autoload stuff
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Zmode - uncompress files
(autoload 'gz-mode "gz-mode" "gunzip files on visit" t)
(setq auto-mode-alist (nconc '(("\\.[Zz]$" . gz-mode)) auto-mode-alist))
(setq auto-mode-alist (nconc '(("\\.gz$" . gz-mode)) auto-mode-alist))

;;
;; compress-buffer
;;
(autoload 'compress-buffer "compress-buffer"
  "Remove trailing whitespace and use more tabs if possible." t)

;;
;; find-this-file
;;
(autoload 'find-this-file "find-this-file"
	  "Find file with file name that pointer is on." t)
(if (fboundp 'find-this-file)
    (global-set-key "\C-cf" 'find-this-file))

;;
;; live-find-file
;;
(autoload 'live-find-file "live" "View a file with \"tail -f\"" t)

(defun my-symbol-around-point ()
  "Return the symbol around the point as a string."
  (save-excursion
    (if (not (looking-at "\\s_\\|\\sw")) ; if not in a symbol
	(re-search-backward "\\s_\\|\\sw" nil t)) ; go into prev. one
    (buffer-substring
      (progn (forward-sexp 1) (point))
      (progn (backward-sexp 1) (point)))))
;;
;; Perl
;;
(setq auto-mode-alist (nconc '(("\\.cgi$" . perl-mode)) auto-mode-alist))
(setq auto-mode-alist (nconc '(("\\.pl$" . perl-mode)) auto-mode-alist))
(setq auto-mode-alist (nconc '(("\\.perl$" . perl-mode)) auto-mode-alist))
(add-hook
 'perl-mode-hook
 '(lambda () (modify-syntax-entry ?\' " " perl-mode-syntax-table)))

(message "emacs-cmn loaded")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; .emacs-cmn.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; eof ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
