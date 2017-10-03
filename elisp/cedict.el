;;; cedict.el --- Utilities for a Chinese to English dictionary

;; Copyright (C) 2000 Yuji 'bmonkey' Minejima <ggb01164@nifty.ne.jp>

;; Author: Yuji Minejima <ggb01164@nifty.ne.jp>
;;         <http://homepage1.nifty.com/bmonkey/>
;; $Revision: 1.1 $
;; Keywords: local

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

;; Utilities for cedict, a Chinese to English dictionary.
;;
;; As of 2000/07/26
;; Cedict's authors homepage: http://www.unc.edu/~pauld/
;; Cedict in BIG5: ftp://ftp.cc.monash.edu.au/pub/nihongo/cedictb5.zip
;; Cedict in GB: ftp://ftp.cc.monash.edu.au/pub/nihongo/cedictgb.zip
;;
;;
;; How to convert cedict to SDIC format.
;; cedict.GB
;; 0: load-library cedict
;; 1: C-x RET C cn-gb-2312
;; 2: C-x C-f cedict.GB
;; 3: M-x cedict-check
;;    Correct malformed records if error occures.
;;    Keep on doing this untill no error occures.
;; 4: M-x cedict-convert-to-sdic

;;; Code:

(defvar cedict-record-regexp
  "^\\([^[ ]+\\( [^[ ]+\\)*\\) *\\[\\([^]]+\\)\\] */\\(.*\\)/\\( \\|ë°°\\)*\n"
  "Regexp to recognize a record of cedict.")

(defvar cedict-entry-number nil)

(defsubst cedict-format-pinyin (cedict-pinyin)
  ""
  (save-match-data
    (let ((syllable-list (split-string cedict-pinyin))
          syllable pinyin)
      (while syllable-list
        (setq syllable (car syllable-list))
        (when (string-match "uu" syllable)
          (setq syllable
                (concat (substring syllable 0 (match-beginning 0))
                        "Å¸"
                        (substring syllable (match-end 0)))))
        (setq pinyin (concat pinyin syllable)
              syllable-list (cdr syllable-list)))
      pinyin)))

(defun cedict-check ()
  ""
  (interactive)
  (forward-line 0)
  (setq cedict-entry-number 1)
  (when (looking-at "CEDICT.*\n")
    (forward-line))
  (while (looking-at cedict-record-regexp)
    (message (format "entry %s" cedict-entry-number))
    (setq cedict-entry-number
          (1+ cedict-entry-number))
    (forward-line))
  (unless (eobp)
    (error "Invalid record found.")))

(defun cedict-convert-to-sdic ()
  ""
  (interactive)
  (let ((out-buf (generate-new-buffer "*cedict.sdic*"))
        chinese pinyin english)
    (goto-char (point-min))
    (setq cedict-entry-number 1)
    (when (looking-at "CEDICT.*\n")
      (let ((copyright (match-string 0)))
        (with-current-buffer out-buf
          (insert copyright))
        (forward-line)))
    (while (looking-at cedict-record-regexp)
      (message (format "entry %s" cedict-entry-number))
      (setq chinese (match-string 1)
            pinyin  (cedict-format-pinyin (match-string 3))
            english (match-string 4))

      (with-current-buffer out-buf
        (insert (concat "<H>" chinese " [" pinyin "]" "</H>"
                        "<K>" chinese "</K>"
                        "<K>" pinyin "</K>"
                        english "\n")))
      (setq cedict-entry-number (1+ cedict-entry-number))
      (forward-line))
    (unless (eobp)
      (error "Invalid record found."))
    (switch-to-buffer out-buf)))

(provide 'cedict)

;;; Local Variables: ***
;;; coding: emacs-mule ***
;;; comment-start: ";;; "  ***
;;; comment-end:"***" ***
;;; End: ***
;;; cedict.el ends here
