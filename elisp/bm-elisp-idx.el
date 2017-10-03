;;; bm-elisp-idx.el --- symbol name index of elisp manual.

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

(require 'bm-elisp-idx-config)

(defun bm-elisp-idx (emacs)
  "Return symbol index (alist) of elisp manual for EMACS."
  (condition-case err
      (require (bm-elisp-idx-config emacs "idx-feature"))
    (error
     (if (y-or-n-p
	  (format "Symbol index for %s is not installed. Install now? " emacs))
	 (progn
	   (eval-and-compile (require 'bm-elisp-idx-make))
	   (bm-elisp-idx-install emacs)
	   (require (bm-elisp-idx-config emacs "idx-feature")))
       (signal (car err) (cdr err)))))
  (eval (bm-elisp-idx-config emacs "idx-var")))
    

(provide 'bm-elisp-idx)

;;; bm-elisp-idx.el ends here
