;;; bm-elisp-idx-config.el --- config for elisp symbol name index.

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
(setq bm-elisp-data-dir "/usr/local/share/emacs/site-lisp/elisp/data")

(defconst bm-elisp-idx-config
  `((fsfemacs
            ("info-file-re" . "^elisp.*$")
	    ("info-file"    . "elisp")
	    ("info-dir"     . "/usr/share/info")
	    ("idx-dir"      . ,bm-elisp-data-dir)
	    ("idx-file"     . "bm-elisp-idx-emacs.el")
	    ("idx-var"      . bm-elisp-idx-emacs)
	    ("idx-feature"  . bm-elisp-idx-emacs)
	    )
    
    (xemacs ("info-file-re" . "^lispref.info.*$")
	    ("info-file"    . "lispref.info")
	    ("info-dir"     . "/usr/share/info/xemacs-20.4")
	    ("idx-dir"      . "/usr/local/share/emacsen/site-lisp/data")
	    ("idx-file"     . ,bm-elisp-data-dir)
	    ("idx-var"      . bm-elisp-idx-xemacs)
	    ("idx-feature"  . bm-elisp-idx-xemacs)
	    ))
  "Alist of information needed to create elisp symbol index.")

(defun bm-elisp-idx-config (emacs attr)
  "Get the value of ATTR for EMACS."
  (let* ((alist (or (assoc emacs bm-elisp-idx-config)
		    (error "Can't find config for %s" emacs))))
    (cdr (assoc attr alist))))




(provide 'bm-elisp-idx-config)

;;; bm-elisp-idx-config.el ends here
