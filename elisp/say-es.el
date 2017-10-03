;;; say-es.el --- skeletal emacs lisp library file

;; Copyright (C) 2000 Yuji 'bmonkey' Minejima <ggb01164@nifty.ne.jp>

;; Author: Yuji Minejima <ggb01164@nifty.ne.jp>
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

;; This is a quick hack to play Spanish word pronunciation.

;;; Code:

(defvar say-es-command "wavp"
  "")

(defvar say-es-dir "/home/bmonkey/sound/spanish/word/"
  "")

(defun say-es-word-to-file (word)
  ;(setq word (downcase word))
  (let ((es-en '((?á . ?a) (?í . ?i) (?ú . ?u) (?é . ?e) (?ó . ?o)
                 (?ñ . ?n) (?\ . ?\_)
                 (?Á . ?A) (?Í . ?I) (?Ú . ?U) (?É . ?E) (?Ó . ?O)
                 (?Ñ . ?N)))
        pair
        (i 0)
        (len (length word))
        name)
    (while (< i len)
      (setq name (concat name
                         (char-to-string
                          (if (setq pair (assoc (aref word i) es-en))
                              (cdr pair)
                            (aref word i)))))
      (setq i (1+ i)))
    (concat name "-s.wav")))

(defun say-es (word)
  (interactive (list (say-es-input (not current-prefix-arg))))
  (let ((file (concat say-es-dir (say-es-word-to-file word)))
        exit-status)
    (cond
     ((file-exists-p file)
      (setq exit-status (call-process say-es-command nil nil nil file))
      (when (not (zerop exit-status))
        (error "%s returned exitstatus %s" say-es-command exit-status)))
     (word
      (message "Sound file for %S not found." word)))))

(defun say-es-word-at-point ()
  (save-excursion
    (buffer-substring-no-properties
     (progn (skip-syntax-backward "w") (point))
     (progn (skip-syntax-forward "w") (point)))))

(defun say-es-input (&optional use-default-not-ask)
  (let* ((default (say-es-word-at-point))
         (minibuffer-setup-hook (append minibuffer-setup-hook
                                        (list (lambda () (interactive)
                                                (activate-input-method
                                                 'latin-1-postfix)))))
         (prompt (format "Say spanish word: %s"
                         (if default (concat "(default " default ") ")
                           "")))
         input)
    (if (and use-default-not-ask default)
        default
      (setq input (read-from-minibuffer prompt))
      (if (and default (or (not input) (string= input "")))
          default
        input))))



(provide 'say-es)

;;; say-es.el ends here
