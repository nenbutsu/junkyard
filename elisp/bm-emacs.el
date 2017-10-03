;;; bm-emacs.el --- define emacs flavor symbol

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

;; Local naming convention for various kinds of Emacsen.
;;
;; variant             Used to denote all versions of emacs developed by
;;                     a specific organization.
;;                     e.g.  fsfemacs, xemacs, mule, ...
;;
;; flavor              Used to denote a specific major version of emacs 
;;                     developed by a specific organization.
;;                     e.g.  fsfemacs19, fsfemacs20, xemacs20, ...
;;


;;; Code:

;;;(defconst bm-emacs-variant-list
;;;  '(fsfemacs xemacs mule)
;;;  "List of Emacs variant name symbol.")

(defconst bm-emacs-variant
  (cond
   ((featurep 'xemacs)      'xemacs)
   ((featurep 'meadow)      'meadow)
   ((fboundp 'mule-version) 'mule)
   (t                       'fsfemacs)
   )
  "Emacs variant name symbol. e.g. fsfemacs, xemacs, mule.")

(defconst bm-emacs-flavor
  (read (concat 
	 (symbol-name bm-emacs-variant)
	 (cond
	  ((eq bm-emacs-variant 'mule)
	   (progn (string-match "[0-9]+" (mule-version))
		  (match-string 0 (mule-version))))
	  (t (number-to-string emacs-major-version)))))
"Emacs flavor symbol consisting of <emacs variant> + <major ver or mule ver>. 
e.g. 'emacs20', 'xemacs20', 'mule2', ...")

	

(provide 'bm-emacs)

;;; bm-emacs.el ends here
