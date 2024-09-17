;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; find-this-file.el --- find the file named nearest the cursor
;;
;; Author          : Rich Berlin
;; Created On      : Tue Aug  30 16:04:32 1988
;; Last Modified By: 
;; Last Modified On: Tue Jun  6 17:48:13 1989
;; Update Count    : 76
;; Status          : Probably safe.  Bug reports to rberlin@sun.com, please.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Edit-mode feature: find the file named named under (near, and
;; always on the same line) the cursor, using path search rules to
;; locate it somewhere in the filesystem.
;;
;; Richard Berlin
;; Sun Microsystems, Corporate Demo Group
;;
;; Thanks to Mark Baushke of Silvar-Lisco for suggestions on how to
;; make this more useful, and to Richard Stallman of FSF for his
;; suggestion on how to generalize it.

;; This file defines buffer-local variables; it should probably be run
;; before any code which changes these variables.  To install in a
;; major mode, the mode must set up include-pathname-regexp, probably
;; search-path-hook, and maybe filename-adjust-hook.

(defvar find-this-file-usually-creates nil
  "*If true, find-this-file will not wait for confirmation before\n\
creating a missing file.")

(defvar buffer-include-path nil
  "*List of directories (strings), the include search path used by find-this-file.")

(make-variable-buffer-local 'buffer-include-path)
(set-default 'buffer-include-path nil)

;;(defvar include-pathname-regexp
;;  "[)(><\"\':\n\t ]\\([^#)(><\"\':\n\t ]+\\)[)(><\"\':\n\t ]"
;;  "Regular expression which describes an include file pathname.\n\
;;The filename itself must be the first grouped item in the regexp.")
;; Removed ':' from first sub-expresion so /some/path/file:30 would
;; find the filename and not the linenumber.
(defvar include-pathname-regexp
  "[)(><\"\'\n\t ]\\([^#)(><\"\':\n\t ]+\\)[)(><\"\':\n\t ]"
  "Regular expression which describes an include file pathname.\n\
The filename itself must be the first grouped item in the regexp.")

;;(make-variable-buffer-local 'include-pathname-regexp)
;;(set-default 'include-pathname-regexp
;;	     "[)(><\"\':\n\t ]\\([^#)(><\"\':\n\t ]+\\)[)(><\"\':\n\t ]")
(make-variable-buffer-local 'include-pathname-regexp)
(set-default 'include-pathname-regexp
	     "[)(><\"\'\n\t ]\\([^#)(><\"\':\n\t ]+\\)[)(><\"\':\n\t ]")

(defvar search-path-hook nil
  "Function used to determine the search path for find-this-file.\n\
Called after an re-search for include-pathname-regexp, so it is allowed\n\
to make assumptions about (match-beginning *) and (match-end *).  If this\n\
hook is nil, only the current directory is searched.")

(make-variable-buffer-local 'search-path-hook)
(set-default 'search-path-hook
	     '(lambda () (cons (concat (directory-file-name default-directory))
			       buffer-include-path)))

(defvar filename-adjust-hook nil
  "Function to adjust the filename used by find-this-file.  Often used\n\
to enforce a particular extension on a filename.")

(make-variable-buffer-local 'filename-adjust-hook)
(set-default 'filename-adjust-hook nil)

;;; The command definintion itself.
(defun find-this-file (other-window)
  "Find the file named under the cursor; with prefix arg, find it in\n\
(an)other window.  The exact behavior depends on the values of the\n\
search-path-hook and include-pathname-regexp in this buffer.  In some\n\
modes, the filename may be adusted (using filename-adjust-hook) before\n\
the search is performed.\n\
\n\
If the specified file does not exist, it will be created in the first\n\
writable directory on the path.  Confirmation is required before the\n\
file will be created, unless find-this-file-usually-creates is non-nil."
  (interactive "P")
  (catch 'error
    (let* (filename file-prefixes name-string
           (find-file-command
            (if other-window 'find-file-other-window 'find-file)))
      (save-excursion
	(let (limit eol pt)
	  (save-excursion (beginning-of-line) (setq limit (1- (point)))
                          (end-of-line) (setq eol (1+ (point))))
          (setq pt (point))
          (if (not (looking-at include-pathname-regexp))
	      (if (or 
		   (and (re-search-forward "[ \t\n]")
			(re-search-backward include-pathname-regexp limit t))
		   (and (goto-char pt)
                        (re-search-backward "[ \t\n]")
			(re-search-forward include-pathname-regexp eol t)))
		  ()
		(goto-char eol)
		(if (re-search-backward include-pathname-regexp limit t)
		    ()
		  (beep)
		  (message "No filename on line?")
		  (sit-for 3)
		  (throw 'error "No filename on line?")))))

	(setq name-string (buffer-substring (match-beginning 1)
					    (match-end 1)))
        (setq file-prefixes
              (if search-path-hook 
		  (funcall search-path-hook)
		'("./")))
	(if filename-adjust-hook
	    (setq name-string (funcall filename-adjust-hook name-string)))

	(setq filename (first-file-in-path file-prefixes name-string)))
      (cond 
       (filename (funcall find-file-command filename)
		 (message "Visiting %s" filename))
       (t (create-file-on-path file-prefixes name-string find-file-command
                               find-this-file-usually-creates))))))

;;; Support code, called by find-this-file.

;;; I used while loops to scan lists in these functions.  Which does
;;; emacs run more effectively, tail recursion or a loop?

;; Search the given path, looking for a file named "filename".  Return
;; the first one found, or nil.
(defun first-file-in-path (path filename)
  (let (this-file)
    (while path
      (setq this-file (concat (car path) "/" filename))
      (if (file-exists-p this-file)
	  (setq path nil) ;; exit the loop
	(setq path (cdr path))
	(setq this-file nil)))
    this-file))

;; Create a file in the first writable directory on path.
(defun create-file-on-path (path filename command confirmed)
  (let (this-file)
    (while path
      (setq this-file (concat (car path) "/" filename))
      (if (file-writable-p this-file)
	  (setq path nil) ;; exit the loop
	(setq path (cdr path))
	(setq this-file nil)))
    (if this-file
	(if (or confirmed (yes-or-no-p (format "Create %s? " this-file)))
	  (progn
	    (funcall command this-file)
	    (message "Created %s" this-file)))
      (beep)
      (message "File %s does not exist, and no writable directories on path."
	       filename)
      (sit-for 1))))

;;;;;;;;;;;;;; The rest of this is hook defs for C and elisp ;;;;;;;;;;;;;;;;;
;; These are regexp and hook definitions which can be used with
;; find-this-file.

;; include-pathname-regexp and hooks for elisp mode
(defconst elisp-mode-include-pathname-regexp 
  "\"\\([^\"\n\t ]+\\)\"")

(defun elisp-mode-search-path ()
  "Function which determines load search path for find-this-file."
  (append buffer-include-path load-path))

;; make sure name-string always ends in .el
(defun elisp-mode-filename-hack (name-string)
  "Function called by find-this-file to enforce the '.el' extension"
  (cond
   ((string-match ".elc$" name-string)
    (setq name-string
	  (substring name-string 0 -1)))
   ((string-match ".el$" name-string))
   (t (setq name-string (concat name-string ".el"))))
  name-string)


;;; include-pathname-regexp and search-path-hook for c-mode
(defconst c-mode-include-pathname-regexp
  "[\"<]\\([^\"<>\n\t ]+\\)[\">]")

;; Note: for VMS the defaults should be:
;; "GNU_CC_INCLUDE:" "SYS$SYSROOT:[SYSLIB.]"

(defconst c-code-includes
  '("/usr/local/lib/gcc-include"
    "/usr/include")
  "List of directories (strings) where standard C include files reside.")

(defconst c++-code-includes
  '("/usr/local/lib/g++-include"	;; GNU C++ specific include files
    "/usr/include/CC"			;; AT&T C++ head files
    "/usr/local/lib/gcc-include"	;; GNU CC specific header files
    "/usr/include")
  "List of directories (strings) where standard C++ include files reside.")

(defun c-mode-search-path ()
  "Function which determines the search path used by find-this-file.
At the end of the search path is /usr/local/lib/gcc-include and
/usr/include. If the file extension is .C or .H, the C++ include
directories are searched just before this, and if the filename is
quoted, './' is searched. The front of the search path is determined
by consulting the compile-command; if no search path can be determined
then buffer-include-path is used instead."
  (let (include-directories c-include-path file-prefixes) ;; Declare file-prefixes locally
    (setq include-directories
          (cond
           ((memq (last-char-in-string buffer-file-name) '(?C ?H))
            c++-code-includes)
           (t
            c-code-includes)))
    (setq c-include-path
          (or 
           (find-c-include-path-from-compile-command)
           buffer-include-path))
    (save-excursion
      (end-of-line)
      (re-search-backward "[\">]")
      (cond
       ((looking-at "\"")
        (setq file-prefixes
              (cons (directory-file-name default-directory)
                    (append include-directories c-include-path))))
       (t
        (setq file-prefixes
              (append include-directories c-include-path)))))
    file-prefixes)) ;; Return file-prefixes

;; Utility functions used by c-mode-search-path

;; Check the compile-command to see if it has -I switches.  If so,
;; we assume it contains the 'true' include path to be used.
(defun find-c-include-path-from-compile-command ()
  (let (path (here 0) cc str)
    (if (not (string-match "make" compile-command))
	(setq cc compile-command)
      (setq str (concat 
		 (substring compile-command
			    (string-match "[^ \t]" compile-command))
			" -n -W " (file-name-nondirectory buffer-file-name)))
      (if (not (string-match "gnumake" str))
	  (setq str (concat "gnu" str)))
      (message "Asking gnumake for compile command...")
      (setq str (results-of-command "/bin/sh" "-c" str))
      (if (setq cc (string-match (file-name-nondirectory buffer-file-name)
				 str))
	  (progn
	    (setq str (substring str cc))
	    (setq cc (substring str 0 (string-match "[^\\]\n" str))))
	(setq cc compile-command)))
    (while (setq here (string-match "-I *\\([^ ]+\\)" cc here))
      (setq str (substring cc
			   (match-beginning 1) (match-end 1)))
      (setq path (cons str path))
      (setq here (1+ here)))
    (reverse path)))

;; Return the last character of the given string
(defun last-char-in-string (string)
   (and (string-to-char (substring string -1))))

(defun results-of-command (command &rest args)
  (let (foo)
    (save-excursion
      (with-output-to-temp-buffer "*results of command*"
	(apply 'call-process command nil standard-output nil args)
	(set-buffer standard-output)
	(setq foo (buffer-substring (point-min) (1- (point-max)))))
      (kill-buffer "*results of command*"))
    foo))

(message "find-this-file loaded")
