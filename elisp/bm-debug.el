;;; bm-debug.el --- local debug settings

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


;;; Code:

(defvar bm-debug-ignored-errors
  '(beginning-of-line beginning-of-buffer end-of-line end-of-buffer end-of-file buffer-read-only file-supersession "^Previous command was not a yank$" "^Minibuffer window is not active$" "^End of history; no next item$" "^Beginning of history; no preceding item$" "^No recursive edit is in progress$" "^Changes to be undone are outside visible portion of buffer$" "^No undo information in this buffer$" "^No further undo information$" "^Save not confirmed$" "^Recover-file cancelled\\.$" "^Cannot switch buffers in a dedicated window$" "^Not at command line$" "^Empty input ring$" "^No history$" "^Not found$" "^Current buffer has no process$" "^No dynamic expansion for .* found$" "^No further dynamic expansion for .* found$" "^No possible abbreviation preceding point$" "^To complete, the point must be after a symbol at least [0-9]* character long\\.$" "^The string \".*\" is too short to be saved as a completion\\.$" "^No more errors\\( yet\\|\\)$" "^NNTP: Connection closed\\.$" "^Node has no Previous$" "^No menu in this node$" "^Node has no Next$" "^No \".*\" in index$" "^No items suitable for an index found in this buffer\\.$" "^This buffer cannot use `imenu-default-create-index-function'$" "^The mode `.*' does not support Imenu$" "^No word found to check!$" "^Cursor not pointing to message$" "^There is no other window$" "^No manpage [0-9]* found$" "^Can't find the .* manpage$" "^No tags table in use; use .* to select one$" "^There is no default tag$" "^No previous tag locations$" "^File .* is not a valid tags table$" "^No \\(more \\|\\)tags \\(matching\\|containing\\) " "^Rerun etags: `.*' not found in " "^All files processed$" "^No .* or .* in progress$" "^File .* not in current tags tables$" "^No tags table loaded" "^Nothing to complete$" "^Errors in diff output. Diff output is in " "^Hmm... I don't see an Ediff command around here...$" "^Undocumented command! Type `G' in Ediff Control Panel to drop a note to the Ediff maintainer$" ": This command runs in Ediff Control Buffer only!$" ": Invalid op in ediff-check-version$" "^ediff-shrink-window-C can be used only for merging jobs$" "^Lost difference info on these directories$" "^This command is inapplicable in the present context$" "^This session group has no parent$" "^Can't hide active session, $" "^Ediff: something wrong--no multiple diffs buffer$" "^Can't make context diff for Session $" "^The patch buffer wasn't found$" "^Aborted$" "^This Ediff session is not part of a session group$" "^No active Ediff sessions or corrupted session registry$" "^No session info in this line$" "^`.*' is not an ordinary file$" "^Patch appears to have failed$" "^Recomputation of differences cancelled$" "^No fine differences in this mode$" "^Lost connection to ancestor buffer...sorry$" "^Not merging with ancestor$" "^Don't know how to toggle read-only in buffer " "Emacs is not running as a window application$" "^This command makes sense only when merging with an ancestor$" "^At end of the difference list$" "^At beginning of the difference list$" "^Nothing saved for diff .* in buffer " "^Buffer is out of sync for file " "^Buffer out of sync for file " "^Output from `diff' not found$" "^You forgot to specify a region in buffer " "^All right. Make up your mind and come back...$" "^Current buffer is not visiting any file$" "^Failed to retrieve revision: $" "^Can't determine display width.$" "^File `.*' does not exist or is not readable$" "^File `.*' is a directory$" "^Buffer .* doesn't exist$" "^Directories . and . are the same: " "^Directory merge aborted$" "^Merge of directory revisions aborted$" "^Buffer .* doesn't exist$" "^There is no file to merge$" "^Version control package .*.el not found. Use vc.el instead$" "^No user options have changed defaults in recent Emacs versions$" "^no previous record$" "^no next record$")
  "")
  







(provide 'bm-debug)

;;; bm-debug.el ends here
