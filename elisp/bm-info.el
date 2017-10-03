;;; bm-info.el --- GNU info utilities

;; Copyright (C) 2000 Yuji 'bmonkey' Minejima <ggb01164@nifty.ne.jp>

;; Author: Yuji Minejima <ggb01164@nifty.ne.jp>
;; $Revision: 1.1 $
;; Keywords: tools

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:


;;; Code:
(require 'info)

(defun bm-info-current-file ()
  "Return current info file name. Can be absolute or relative path.
e.g. dir, /usr/info/gdb.info, ..."
  (let ((info-buffer (get-buffer "*info*")))
    (when info-buffer (with-current-buffer info-buffer Info-current-file))))

(defun bm-info-current-node ()
  "Return current info node name."
  (let ((info-buffer (get-buffer "*info*")))
    (when info-buffer (with-current-buffer info-buffer Info-current-node))))

(defun bm-info-current-node-spec ()
  "Return current info node specification. e.g. \"(emacs)Top\""
  (interactive)
  (let* ((file (bm-info-current-file))
	 (node (bm-info-current-node)))
    (when (and file node)
      (setq file (file-name-nondirectory file))
      (save-match-data
	(if (string-match "^emacs-e20\\(.\\)" file)
	    (setq file (concat "emacs" (match-string 1 file)))))
      (format "(%s)%s" file node))))

;;;###autoload
(defun bm-insert-info-link ()
  "Insert Info node link."
  (interactive)
  (let ((filenode (or (bm-info-current-node-spec)
		      (error "Info node not found.")))
	(original-point (point)))
    (bm-delete-syntax-char-within-line " ")
    ;;(insert (format "[|\"%s\"|]" filenode))
    (insert (format "\"%s\"" filenode))
    (goto-char original-point)
    (bm-right-justify-from-here)))


(defun bm-link-conv-info ()
  "Convert old link [|(emacs:top)|] to [|\"(emacs)top\"|]."
  (interactive)
  (while (re-search-forward
	  "\\[|(\\([^:)]+\\)\\(:\\([^)]+\\)\\)?)\\([^|]*\\)|\\]"
	  (point-max) t)
    (set-mark (car (match-data)))
    (replace-match "[|\"(\\1)\\3\"\\4|]")
    (when (y-or-n-p "Indent? ")
      (search-backward "[|\"(")
      (bm-right-justify-from-here))))










(provide 'bm-info)

;;; bm-info.el ends here
