;;; bm-find-elisp.el -- show elisp information.
;;  Copyright (C) bmonkey <ggb01164@nifty.ne.jp>
;;
;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;;; Commentary:

;; Ask user for a emacs lisp function/variable,
;; then go to the definition of that object
;; in either the emacs lisp manual info page or library file.


;;; Code:
(require 'bm-emacs)
(require 'info)
(require 'info-look)
(require 'find-func)
(eval-when-compile (require 'cl))

;;;###autoload
(defun bm-find-elisp-symbol (name)
  ""
  (interactive (list (bm-find-elisp-read-symbol)))
  (eval-and-compile (require 'bm-elisp-idx))
  (let* ((symbol (intern-soft name))
         (interned (string= name (symbol-name symbol))))
    (cond
     ;;; Comment out to use info-lookup instead of bm-elisp-idx
     ;;;((setq cell (assoc name (bm-elisp-idx bm-emacs-variant)))
     ;;; (eval-and-compile (require 'info))
     ;;; (if (and (fboundp 'info-other-window)
     ;;;          (or (eq major-mode 'emacs-lisp-mode)
     ;;;   	   (eq major-mode 'lisp-interaction-mode))) 
     ;;;     (info-other-window))
     ;;; (Info-find-node (bm-elisp-idx-config bm-emacs-variant "info-file")
     ;;;   	      (cdr cell))
     ;;; (re-search-forward 
     ;;;  (concat "^ - [^:]+: " (regexp-quote name) "\\(\n\\| \\)")))

     ((condition-case nil               ; info-lookup
          (progn (info-lookup-symbol name 'emacs-lisp-mode) t)
        (error nil)))
     ((and interned
           (or
            (and (fboundp symbol)
                 (or (condition-case nil
                         (progn (find-function symbol) t) (error nil))
                     (condition-case nil
                         (progn (describe-function symbol) t) (error nil))))
            (and (boundp symbol)
                 (or (condition-case nil
                         (progn (find-variable symbol) t) (error nil))
                     (condition-case nil
                         (progn (describe-variable symbol) t) (error nil)))))))
     ((condition-case nil
	  (progn (bm-find-elisp-find-tag name) t) (error nil)))
     (t (error "Unknown symbol: %s" name)))))


(defun bm-find-elisp-read-symbol ()
  "Read a symbol name using the minibuffer."
  (eval-and-compile (require 'thingatpt))
  (eval-and-compile (require 'bm-complete))
  (let ((original-syntax-table (syntax-table)))
    (set-syntax-table emacs-lisp-mode-syntax-table)
    (unwind-protect
	(let* ((completion-ignore-case t)
	       (default (thing-at-point 'symbol))
	       (prompt 
		(bm-complete-default-prompt "Find elisp symbol: " default)))
	  (completing-read prompt
                           obarray
                           #'(lambda (symbol)
                               (or (boundp symbol)
                                   (fboundp symbol)))
                           nil          ;require-match
                           nil          ;initial
                           nil          ;hist
                           default))
      (set-syntax-table original-syntax-table))))

;;; for completing-help
(defun bm-find-elisp-read-symbol-p ()
  "for completing-help.el"
  (eq completing-help-completing-read-command 'bm-find-elisp-symbol))

(defun bm-find-elisp-get-info (name)
  ""
  (let ((symbol (intern name)))
    (or (and (fboundp symbol) (documentation symbol))
        (and (boundp  symbol) (documentation-property
                               symbol 'variable-documentation))
        "")))

(defvar bm-find-elisp-group
  '(:predicate  bm-find-elisp-read-symbol-p
    :get        bm-find-elisp-get-info)
  "")

(if (featurep 'completing-help)
    (add-to-list 'completing-help-groups 'bm-find-elisp-group)
  (add-hook 'completing-help-load-hook
            #'(lambda () (add-to-list 'completing-help-groups
                                      'bm-find-elisp-group))))

;;;###autoload
(defun bm-find-elisp-file (file)
  ""
  (interactive (list (bm-find-elisp-read-file)))
  (let (cell)
    (cond
     ((setq cell (assoc file bm-elisp-file-idx))
      (find-file (expand-file-name file (cdr cell))))
     (t (save-match-data
	  (cond
	   ((string-match "\\.elc$" file)
	    (setq file (substring file 0 (1- (length file)))))

	   ((not (string-match "\\.el$" file))
	    (setq file (concat file ".el"))))

	  (find-file (or (locate-library file)
			 (error "No library %s in search path" file)))))
     )))
      

(defun bm-find-elisp-read-file ()
  "Read a elisp file name using the minibuffer."
  (eval-and-compile (require 'thingatpt))
  (eval-and-compile (require 'bm-complete))
  (eval-and-compile (require 'bm-elisp-file-idx))
  (let* ((completion-ignore-case t)
	 (default (thing-at-point 'filename))
	 (prompt (bm-complete-default-prompt "Find elisp file: " default)))
    (bm-completing-read prompt
			bm-elisp-file-idx
			nil		;predicate
			t		;require-match
			nil		;initial
			nil		;hist
			default)))

(defun bm-find-elisp-find-tag (tagname)
  "find-tag wrapper for Emacs and XEmacs compatibility."
  (eval-and-compile (require 'etags))
  (if (featurep 'xemacs)
      (find-tag (concat "\\_" tagname "\\_"))
    (find-tag (format "(def[^ ]+ %s " (regexp-quote tagname)) nil t)))

(provide 'bm-find-elisp)

;;; bm-find-elisp.el ends here
