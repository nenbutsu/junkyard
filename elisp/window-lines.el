;; basic cursor moving commands based on window lines
;; Copyright (C) 1990 enami tsugutomo (enami@ptgd.sony.co.jp)

;; This file is not a part of GNU Emacs.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; This file, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;;
;; $Id: window-lines.el,v 1.1 2004-06-05 08:05:19 yuji Exp $
;;

(defvar cursor-move-window-line nil "*T if cursor move on window lines.")

(defun window-line-move (arg)
  (if (not (memq last-command
		 ;; Do you think also need next-line and previous-line ? 
		 '(window-next-line window-previous-line) ))
      ;; update temporary-goal-column
      (setq temporary-goal-column
	    (if (and track-eol (eolp))
		9999
	      (current-window-column) )))
  ;; move vertically
  (forward-window-line arg)
  ;; adjust column
  (move-to-window-column (or goal-column temporary-goal-column))
  nil)


;;;
;;; basic functions
;;;
(defun beginning-of-window-line ()
  "Goto beginning of current window line."
  (vertical-motion 0))

(defun end-of-window-line ()
  "Goto beginning of current window line."
  (if (eobp)
      nil				; already on eol.
    (vertical-motion 1)
    ;; never back if last line ends without LFD.
    (if (or (not (eobp))
	    (= (current-column) 0))	; If !0, it means was on last line
					; and reach to end of buffer
					; Attention that not on the eob
					; at first.
	(forward-char -1) )))

(defun current-window-column ()
  "Return the horisontal position of point on window."
  (let* ((ccol (current-column))
	 ;; left column
	 (lcol (save-excursion
		 ;; moves left edge of current line
		 (beginning-of-window-line)
		 (current-column) )))
    ;; if selective-display some times become negative, incorrect value
    ;; is returne but no way to get correct value, I think ... :-<
    (- ccol lcol) ))

(defun move-to-window-column (col)
  "Move point to column COLUMN in the current window line."
  (let* ((beg (progn
		(beginning-of-window-line)
		(current-column)))
	 (lim (save-excursion
		(end-of-window-line)
		(current-window-column) )))
    (and (< col 0) (setq col 0))
    (and (< lim col)
	 (< 0 lim)			; for selective display. DOUBTFUL.
	 (setq col lim))
    (move-to-column (+ beg col)) ))

(defun forward-window-line (args)
  "Move ARGS line on window with preserving current window column."
  (if (/= args 0)
      (let* ((cwcol (current-window-column)))
	(vertical-motion args)
	(move-to-window-column cwcol) )))


;;;
;;; editing commands
;;;
(defun window-next-line (arg)
  "Move ARGs line down.  Almost same as next-line but move on window line."
  (interactive "p")
  (if (and cursor-move-window-line (not truncate-lines))
      (if (/= arg 1)
	  (window-line-move arg)
	;; arg is 1. special case.
	(let* ((point (point)))
	  (window-line-move 1)
	  ;; (window-line-move 1) doesn't move if on the last line.
	  (if (/= point (point))
	      nil
	    ;; on the last line
;;	    (end-of-window-line)	最終行で改行しないようにした(90/07/05)
;;	    (insert ?\n)			isoyama@mm.rd.nttdata.jp
	    )))
    (setq this-command 'next-line)	;for line-move
    (next-line arg) ))
	   
(defun window-previous-line (arg)
  "Move ARGs line up.  Almost same as previous-line but move on window line."
  (interactive "p")
  (if (and cursor-move-window-line (not truncate-lines))
      (window-line-move (- arg))
    (setq this-command 'previous-line)	;for line-move
    (previous-line arg) ))

(defun window-beginning-or-end-of-line (new org arg)
  (if (not cursor-move-window-line)
      (funcall org arg)
    ;; first move (1- arg) line forward
    (forward-window-line (1- arg))
    (funcall new)))

(defun window-beginning-of-line (arg)
  "Move to beginning of line.  See also the documentation of
beginning-of-line."
  (interactive "p")
  (window-beginning-or-end-of-line
   'beginning-of-window-line 'beginning-of-line arg))

(defun window-end-of-line (arg)
  "Move to end of line.  See also the documentation of end-of-line."
  (interactive "p")
  (window-beginning-or-end-of-line
   'end-of-window-line 'end-of-line arg))

;;
;; minor mode definitions
;;
(or (assq 'cursor-move-window-line minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(cursor-move-window-line " W")	;; <- " WLine"
		minor-mode-alist) ))

(defvar window-line-mode-hook
  (list (function
	 (lambda ()
	   (and cursor-move-window-line
		(mapcar
		 (function (lambda (pair) 
			     (apply (function substitute-all-key-definition)
				    pair) ))
		 '((next-line  window-next-line)
		   (previous-line window-previous-line)
		   (beginning-of-line window-beginning-of-line)
		   (end-of-line window-end-of-line)) )))))
  "List of hook variable which value is called as no arg function
when window-line-mode is executed.")

(defun substitute-all-key-definition (olddef newdef)
  "Replace OLDDEF with NEWDEF by defining local key map."
  (let* (map key (keys (where-is-internal olddef (current-local-map))))
    (while keys
      (setq key (car keys)
	    keys (cdr keys))
      (local-set-key key newdef) )))

(defun window-line-mode (arg)
  "Toggle window line mode. With positive ARGument, turn on, else off.
At the end, window-line-mode-hook is called.  See run-hooks for hook variable."
  (interactive "P")
  (make-local-variable 'cursor-move-window-line)
  (setq cursor-move-window-line
	(if (null arg)
	    (not cursor-move-window-line)
	  (< 0 (prefix-numeric-value arg)) ))
  (set-buffer-modified-p (buffer-modified-p)) ; update mode-line
  (run-hooks 'window-line-mode-hook) )


;; (mapcar (function
;; 	 (lambda (args)
;; 	   (local-set-key (car args) (cdr args))))
;; 	'(("\C-a" . window-beginning-of-line)
;; 	  ("\C-e" . window-end-of-line)
;; 	  ("\C-n" . window-next-line)
;; 	  ("\C-p" . window-previous-line)
;; 	  ))
