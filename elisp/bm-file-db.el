;;; bm-file-db.el --- local file location database.

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

(defun bm-file-db-id2file (id)
  (let* ((exit-status)
	 (cmd (format "fdb_id2file '%s'" id))
	 (file (with-output-to-string
		 (with-current-buffer standard-output
		   (setq exit-status
			 (call-process
			  shell-file-name
			  nil t nil shell-command-switch cmd))))))
    (save-match-data
      (cond
       ((> exit-status 0)
	(error "Shell command `%s' failed: %s" cmd exit-status))

       ((not (string-match "^\\([^\x00-\x20]+\\)[\n\r]*$" file))
	(error "Shell command `%s' returned invalid file name: %s" cmd file))

       (t (match-string 1 file))))))
  







(provide 'bm-file-db)

;;; bm-file-db.el ends here
