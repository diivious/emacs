; Path: dg-rtp!rock!mcnc!stanford.edu!snorkelwacker.mit.edu!think.com!spool.mu.edu!uwm.edu!ux1.cso.uiuc.edu!m.cs.uiuc.edu!sane
; From: sane@cs.uiuc.edu (Aamod Sane)
; Newsgroups: gnu.emacs.help,comp.emacs
; Subject: Tracking-shell (Re: multiple concurrent emacs shells?)
; Date: 21 Jul 91 21:12:29 GMT
; References: <1991Jul20.014343.11662@milton.u.washington.edu>
; Organization: University of Illinois, Dept. of Comp. Sci., Urbana, IL
; Nntp-Posting-Host: clitus.cs.uiuc.edu
; 
; narf@milton.u.washington.edu (Francis Taylor) writes:
; 
; >Does anyone out there know of any way to have multiple running shells
; >in one emacs session?  Thanks.
; 
; Here is a lisp file that will do it automatically for you. REquires
; cmushell , which is a better shell than normal shell. You will
; be find it at any archive, prep.ai.mit.edu or tut.cis.ohio-state.edu etc.
; 
; multi-shell makes multiple shells , tracking-shell  when invoked from
; a buffer will already do a cd to the buffer disrectory. Usually,
; you do not require multiple shells at all in this case.
; You can change this to use shell if you want
; 
; From tracking shell comments:
; 
;; When invoked from a buffer, Tracking-Shell cd's to that buffers directory.
;; Thus to do something in the current directory of a buffer, pop up
;; a tracking-shell, do whatever, and close the shell. Automatic cd'ing
;; allows direct usage of shell commands; complicated key sequences are
;; not neccessary for file manipulation.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;    Multiple Simultaneous shell 
;;    Trakcing shell - track the directory of buffer every time
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copyright (C) Aamod Sane (sane@cs.uiuc.edu)
;; Author: Aamod Sane
;; This file is not part of GNU Emacs yet.
;; The FSF copyright below applies to this file.

;; LCD Archive Entry:
;; multi-shell|Aamod Sane|sane@cs.uiuc.edu
;; |Multiple simultaneous shells
;; |91-07-21||~/misc/multi-shell.el.Z|

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

(defun multi-shell (&optional n)
  "Invoke a new shell you invoke.See documentation for `shell-mode' for details."
  (interactive)
  (let* ((name (if (null n) "MultiShell" (format "MultiShell-%s" n))) ;; Use descriptive name format for numbered shells
         (buf (generate-new-buffer name))
         (bufnam (buffer-name buf)))
    (unless (comint-check-proc bufnam)
      (let* ((prog (or explicit-shell-file-name
                       (getenv "ESHELL")
                       (getenv "SHELL")
                       "/bin/sh"))
             (shell-name (file-name-nondirectory prog)) ;; Rename to avoid collision with outer `name`
             (startfile (concat "~/.emacs_" shell-name))
             (xargs-name (intern-soft (concat "explicit-" shell-name "-args"))))
        (with-current-buffer (apply 'multi-comint (list bufnam prog
                                                       (if (file-exists-p startfile) startfile nil)
                                                       (if (and xargs-name (boundp xargs-name))
                                                           (symbol-value xargs-name)
                                                         '("-i"))))
          (shell-mode)))) ;; Use shell-mode instead of cmushell-mode
    bufnam))

(defun multi-comint (name program &optional startfile &rest switches)
  "Create a comint process in a buffer named NAME running PROGRAM.
Optional argument STARTFILE is the file to source in the new shell.
SWITCHES are passed to the shell."
  (let* ((buffer (get-buffer-create name))
         (proc (get-buffer-process buffer)))
    (unless (and proc (memq (process-status proc) '(run stop)))
      (with-current-buffer buffer
        (comint-mode) ;; Set comint-mode and initialize buffer
        (comint-exec buffer name program startfile switches)))
    buffer))

(defconst tracking-shell-name "*Tracking-Shell*"
  "Name of the tracking shell buffer.")

(defconst tracking-shell-name-nostar "Tracking-Shell"
  "Name of the tracking shell buffer without the star.")

(defvar tracking-shell-other-window t
  "Whether tracking shell should appear in current window or not.")

(defun tracking-shell ()
  "Switch to tracking shell, Sync shell's dir with buffer if shell isrunning."
  (interactive)
  (let ((tracking-shell-buffer (get-buffer tracking-shell-name)))
    (unless (comint-check-proc tracking-shell-name)
      (let* ((prog (or explicit-shell-file-name
                       (getenv "ESHELL")
                       (getenv "SHELL")
                       "/bin/sh"))
             (name (file-name-nondirectory prog))
             (startfile (concat "~/.emacs_" name))
             (xargs-name (intern-soft (concat "explicit-" name "-args"))))
        (with-current-buffer (apply 'make-comint tracking-shell-name-nostar prog
                                    (if (file-exists-p startfile) startfile nil)
                                    (if (and xargs-name (boundp xargs-name))
                                        (symbol-value xargs-name)
                                      '("-i"))))
          (shell-mode)))) ;; Create shell if not running
    (let ((dir default-directory))
      (comint-send-string (get-buffer-process tracking-shell-name)
                          (concat "cd " dir "\n"))
      (with-current-buffer tracking-shell-name
        (cd dir))) ;; Update directory
  ;; Switch to the shell buffer
  (if tracking-shell-other-window
      (switch-to-buffer-other-window tracking-shell-name)
    (switch-to-buffer tracking-shell-name)))

(provide 'multishell)

(message "multi-shell loaded")
