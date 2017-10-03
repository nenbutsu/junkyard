;;; bm-tags-make.el --- skeletal emacs lisp library file

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

;;;###autoload
(defun bm-tags-make (&optional elisp-list &rest opt-list)
  "Make tag file of elisp libraries using etags.
e.g.  After installing new emacs version, the following commands will
      generate appropreate TAGS file.
          1: \\[find-file] ~/ RET
          2: M-x bm-make-tags RET
          3: su -c 'mv TAGS <TAGS-DIR>'
             (<TAGS-DIR> is /usr/share/emacs for GNU Emacs,
                            /usr/lib/xemacs-20.4 for XEmacs 20.4)
"
  (interactive)
  (when (null elisp-list)
    (require 'bm-elisp-file-idx)
    (setq elisp-list (mapcar
		      (lambda (cell)
			(expand-file-name (car cell) (cdr cell)))
		      (bm-elisp-file-idx))))

  (message "Executing etags...")
  (with-temp-buffer
    (let ((etags (apply 'start-process 
			"*etags-process*"
			(current-buffer)
			"xargs"
			(concat "etags." (symbol-name bm-emacs-flavor))
			"-a"
			opt-list)))
      (while elisp-list
	(process-send-string etags (concat (car elisp-list) "\n"))
	(setq elisp-list (cdr elisp-list)))
      (process-send-eof etags)
      (while (member (process-status etags) '(run stop))
	(sleep-for 3))
      (cond
       ((eq (process-status etags) 'signal)
	(message "Executing etags... killed by signal %d"
		 (process-exit-status etags)))
       ((eq (process-status etags) 'exit)
	(if (zerop (process-exit-status etags))
	    (message "Executing etags... finished.")
	  (message "Executing etags... FAILED (status %d)."
		   (process-exit-status etags))))))))
	









(provide 'bm-tags-make)

;;; bm-tags-make.el ends here
