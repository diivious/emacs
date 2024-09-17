;;; show-wspace.el --- Highlight whitespace of various kinds.
;;
;; Filename: show-wspace.el
;; Description: Highlight whitespace of various kinds.
;; Author: Peter Steiner <unistein@isbe.ch>, Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2000-2007, Drew Adams, all rights reserved.
;; Created: Wed Jun 21 08:54:53 2000
;; Version: 21.0
;; Last-Updated: Fri Jan 19 21:25:32 2007 (-28800 Pacific Standard Time)
;;           By: dradams
;;     Update #: 233
;; URL: http://www.emacswiki.org/cgi-bin/wiki/show-wspace.el
;; Keywords: highlight, whitespace
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Highlight whitespace of various kinds.
;;
;; To use this library:
;;
;;    Add this to your initialization file (~/.emacs or ~/_emacs):
;;
;;      (require 'show-wspace) ; Load this library.
;;
;; Then you can use commands `toggle-*' (see below) to turn the
;; various kinds of whitespace highlighting on and off in Font-Lock
;; mode.
;;
;; If you want to always use a particular kind of whitespace
;; highlighting, by default, then add the corresponding `highlight-*'
;; command (see below) to the hook `font-lock-mode-hook'.  Then,
;; whenever Font-Lock mode is turned on, so will the whitespace
;; highlighting.
;;
;; For example, you can turn on tab highlighting by default by adding
;; command `highlight-tabs' to `font-lock-mode-hook' in your .emacs
;; file, as follows:
;;
;;     (add-hook 'font-lock-mode-hook 'highlight-tabs)
;;
;;
;; Faces defined here:
;;    `pesche-space', `pesche-hardspace', `pesche-tab'.
;;
;; Commands defined here:
;;    `toggle-space-font-lock', `toggle-hardspace-font-lock', `toggle-tabs-font-lock',
;;    `toggle-trailing-whitespace-font-lock'.
;;
;; Non-interactive functions defined here:
;;    `highlight-spaces', `highlight-hard-spaces', `highlight-tabs',
;;    `highlight-trailing-whitespace'.
;;
;; Internal variables defined here:
;;
;;    `highlight-hard-spaces-p', `highlight-spaces-p', `highlight-tabs-p',
;;    `highlight-trailing-whitespace-p'.
;;
;; Drew Adams wrote the `toggle-*' commands and `*-p' variables.
;;
;; Peter Steiner wrote the original code that did the equivalent of
;; the `highlight-*' commands here in his `hilite-trail.el'.  The
;; names "pesche" are his.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defgroup Show-Whitespace nil
  "Highlight whitespace of various kinds."
  :group 'convenience :group 'matching
  :link `(url-link :tag "Send Bug Report"
                   ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\show-wspace.el bug: \
&body=describe bug here, starting with `emacs -q'.  \
don't forget to mention your emacs and library versions."))
  :link '(url-link :tag "other libraries by drew"
                   "http://www.emacswiki.org/cgi-bin/wiki/drewselisplibraries")
  :link '(url-link :tag "download"
                   "http://www.emacswiki.org/cgi-bin/wiki/show-wspace.el")
  :link '(url-link :tag "description"
                   "http://www.emacswiki.org/cgi-bin/wiki/showwhitespace#showwspace")
  :link '(emacs-commentary-link :tag "commentary" "show-wspace"))

(defface pesche-tab
  '((t (:background "#e6e6fa"))) ;; hex code for 'lavender'
  "face for highlighting tab characters (`c-i') in font-lock mode."
  :group 'show-whitespace :group 'font-lock :group 'faces)

(defface pesche-space
  '((t (:background "#fff0f5"))) ;; hex code for 'lavender blush'
  "face for highlighting whitespace at line ends in font-lock mode."
  :group 'show-whitespace :group 'font-lock :group 'faces)

(defface pesche-hardspace
  '((t (:background "#98fb98"))) ;; hex code for 'palegreen'
  "face for highlighting non-breaking spaces (`\240') in font-lock mode."
  :group 'show-whitespace :group 'font-lock :group 'faces)

(defvar highlight-tabs-p nil
  "non-nil means font-lock mode highlights tab characters (`c-i').")

(defvar highlight-trailing-whitespace-p nil
  "non-nil means font-lock mode highlights whitespace at line ends.")

(defvar highlight-spaces-p nil
  "non-nil means font-lock mode highlights spaces (`\40').")

(defvar highlight-hard-spaces-p nil
  "non-nil means font-lock mode highlights non-breaking spaces (`\240').")

(defun toggle-tabs-font-lock ()
  "Toggle highlighting of tabs, using face `pesche-tab'."
  (interactive)
  (if highlight-tabs-p
      (progn
        (font-lock-remove-keywords nil '(("[\t]+" (0 'pesche-tab prepend))))
        (remove-hook 'font-lock-mode-hook 'highlight-tabs t))
    (add-hook 'font-lock-mode-hook 'highlight-tabs nil t)
    (highlight-tabs))
  (setq highlight-tabs-p (not highlight-tabs-p))
  (font-lock-flush)
  (message "Tab highlighting is now %s." (if highlight-tabs-p "on" "off")))

(defun toggle-space-font-lock ()
  "Toggle highlighting of space characters (`\40')."
  (interactive)
  (if highlight-spaces-p
      (progn
        (font-lock-remove-keywords nil '(("[\040]+" (0 'pesche-space t))))
        (remove-hook 'font-lock-mode-hook 'highlight-spaces t))
    (add-hook 'font-lock-mode-hook 'highlight-spaces nil t)
    (highlight-spaces))
  (setq highlight-spaces-p (not highlight-spaces-p))
  (font-lock-flush)
  (message "Space highlighting is now %s." (if highlight-spaces-p "on" "off")))

(defun toggle-trailing-whitespace-font-lock ()
  "Toggle highlighting of trailing whitespace."
  (interactive)
  (if highlight-trailing-whitespace-p
      (progn
        (font-lock-remove-keywords nil '(("[\240\040\t]+$" (0 'pesche-space t))))
        (remove-hook 'font-lock-mode-hook 'highlight-trailing-whitespace t))
    (add-hook 'font-lock-mode-hook 'highlight-trailing-whitespace nil t)
    (highlight-trailing-whitespace))
  (setq highlight-trailing-whitespace-p (not highlight-trailing-whitespace-p))
  (font-lock-flush)
  (message "Trailing whitespace highlighting is now %s."
           (if highlight-trailing-whitespace-p "on" "off")))

(defun toggle-hardspace-font-lock ()
  "Toggle highlighting of non-breaking space characters (`\240')."
  (interactive)
  (if highlight-hard-spaces-p
      (progn
        (font-lock-remove-keywords nil '(("[\240]+" (0 'pesche-hardspace t))))
        (remove-hook 'font-lock-mode-hook 'highlight-hard-spaces t))
    (add-hook 'font-lock-mode-hook 'highlight-hard-spaces nil t)
    (highlight-hard-spaces))
  (setq highlight-hard-spaces-p (not highlight-hard-spaces-p))
  (font-lock-flush)
  (message "Hard (non-breaking) space highlighting is now %s."
           (if highlight-hard-spaces-p "on" "off")))


(defun highlight-tabs ()
  "Highlight tab characters (`C-i')."
  (font-lock-add-keywords nil '(("[\t]+" (0 'pesche-tab prepend))) t)
  (font-lock-flush)
  (font-lock-ensure))

(defun highlight-spaces ()
  "Highlight space characters (`\40')."
  (font-lock-add-keywords nil '(("[\040]+" (0 'pesche-space t))) t)
  (font-lock-flush)
  (font-lock-ensure))

(defun highlight-hard-spaces ()
  "Highlight hard (non-breaking) space characters (`\240')."
  (font-lock-add-keywords nil '(("[\240]+" (0 'pesche-hardspace t))) t)
  (font-lock-flush)
  (font-lock-ensure))

(defun highlight-trailing-whitespace ()
  "Highlight whitespace characters at line ends."
  (font-lock-add-keywords nil '(("[\240\040\t]+$" (0 'pesche-space t))) t)
  (font-lock-flush)
  (font-lock-ensure))

;;;;;;;;;;;;;;;;;;;;;;;
(provide 'show-wspace)

(message "show-wspace loaded")
