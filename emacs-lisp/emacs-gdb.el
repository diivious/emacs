;;
;; gdb.....
;;
;Some gdb stuff.
(require 'gud)

;;
(defvar grep-regexp-alist nil)
(defvar gdb-command-name nil)
(defvar gud-gdb-history nil)

(defvar compile-command "-k -j8 ")
(defvar make-command "-k -j8 ")

(setq gud-chdir-before-run nil)
(setq gdb-command-name '("gdb -i=mi"))
(setq gud-gdb-history '("gdb -i=mi eigprd"))

;;
;; make
;;
(global-set-key "\M-\r" 'make)
(defun make (command)
  "Run make, with user-specified args, and collect output in a buffer."
  (interactive (list (read-string "Run make (with args): " make-command)))
  (dvs-compile-start (concat "time make " command)
		     'compilation-mode "make" grep-regexp-alist))

;;
(message "emacs-gdb loaded")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; .emacs-gdb.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; eof ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
