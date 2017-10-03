;;; bm-bookmark-dir.el -- Bookmark perl document
;;  Copyright (C) bmonkey <ggb01164@nifty.ne.jp>
;;
;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;;; Commentary:

;; Bookmark perl document


;;; History:
;;

;;; Code:
(require 'bookmark)
(require 'bm-elisp-util)
(require 'bm-file-db)

;;;###autoload
(defun bm-bookmark-perl ()
  "Bookmark perl document under directory."
  (interactive)
  (bm-bookmark-dir (bm-file-db-id2file 35) "perl.+" "\\([^.]+\\)" 1))

;;;###autoload
(defun bm-bookmark-memo ()
  "Bookmark memo."
  (interactive)
  (bm-bookmark-dir (bm-file-db-id2file 36) ".+\\.txt$" ".+" 0))

(defvar bm-bookmark-overridden-list
  nil
"Used by bm-bookmark-dir to record bookmark collision.")

;;;###autoload
(defun bm-bookmark-dir (dir file-re name-re name-re-pos)
  "Bookmark files matching FILE-RE under DIR.
Bookmark name is determined by matching NAME-RE with file name and
take the NAME-RE-POS sub expression of the matching result.
e.g. (bm-bookmark-dir \"/home/doc/info/memo\" \".+\\.txt$\" \".+\" 0)
     Register bookmark for files under dir /home/doc/info/memo 
     whose name end with '.txt'.
     Bookmark names are file names themselves (without dir part).
"
  (interactive "DBookmark dir: 
sFile regexp: 
sBookmark name regexp (matched against filename): 
nBookmark name regexp pos: (0 = whole, 1 = 1st paren, ...) ")
  (bookmark-bmenu-list)
  (let ((file-list (nreverse (directory-files dir nil file-re)))
	abs-filename
	filename
	name)
    (save-match-data
      (while file-list
	(setq filename     (car file-list)
	      abs-filename (expand-file-name filename dir)
	      name         (progn (string-match name-re filename)
				  (or (match-string name-re-pos filename)
				      filename)))

	(bm-with-file-visiting-buffer abs-filename
	  (if (bookmark-get-bookmark name)
	      (cond
	       ((string= (bookmark-get-filename name) abs-filename)
		(message "Delete old bookmark: %s" name)
		(bookmark-delete name "BATCH"))

	       (t (message "Overriding already existing bookmark %s." name)
		  (setq bm-bookmark-overridden-list
			(cons (cons name (bookmark-get-filename name))
			      bm-bookmark-overridden-list)))))

	  (message "Set bookmark %s for %s" name filename)
          (bookmark-make name))

	(setq file-list (cdr file-list)))))
  (bookmark-bmenu-surreptitiously-rebuild-list)
  (switch-to-buffer "*Bookmark List*")
  (goto-char bookmark-current-point))


(provide 'bm-bookmark-dir)

;;; bm-bookmark-dir.el ends here
