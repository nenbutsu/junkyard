;;; bm-uri.el --- URI utilities. see RFC2396, [|{uri.txt}|]

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

;;-----------------------------------------------
;; URI Reference Regexp. see [|rfc2396 "^A\."|]
;;-----------------------------------------------
(defvar bm-uri-alphanum-re
  "[A-Za-z0-9]")

(defvar bm-uri-escaped-re
  "\\(%[0-9A-Fa-f][0-9A-Fa-f]\\)")

(defvar bm-uri-mark-re
  "[_.!~*'()-]")

(defvar bm-uri-unreserved-re
  "[A-Za-z0-9_.!~*'()---]") ;;  bm-uri-alphanum-re + bm-uri-mark-re

(defvar bm-uri-reserved-re
  "[;/?:@&=+$,]"
  "Characters consisting `reserved' non-terminal symbol in URI spec.")

(defvar bm-uri-uric-re
  "\\([;/?:@&=+$,A-Za-z0-9_.!~*'()---]\\|%[0-9A-Fa-f][0-9A-Fa-f]\\)")
  ;;  bm-uri-reserved-re | bm-uri-unreserved-re | bm-uri-escaped-re

(defvar bm-uri-no-uric-re
  "\\([^%;/?:@&=+$,A-Za-z0-9_.!~*'()---]\\)")

(defvar bm-uri-uric-no-slash-re
  (format "\\(%s\\|%s\\|[;?:@&=+$,]\\)"
	  bm-uri-unreserved-re bm-uri-escaped-re))

(defvar bm-uri-fragment-re
  (format "\\(%s*\\)" bm-uri-uric-re))

(defvar bm-uri-query-re
  bm-uri-fragment-re)

(defvar bm-uri-pchar-re
  (format "\\(%s\\|%s\\|[:@&=+$,]\\)"
	  bm-uri-unreserved-re bm-uri-escaped-re))

(defvar bm-uri-param-re
  (format "\\(%s*\\)" bm-uri-pchar-re))

(defvar bm-uri-segment-re
  (format "\\(%s*\\(;%s\\)*\\)"
	  bm-uri-pchar-re bm-uri-param-re))

(defvar bm-uri-path-segments-re
  (format "\\(%s\\(/%s\\)*\\)"
	  bm-uri-segment-re bm-uri-segment-re))

(defvar bm-uri-rel-segment-re
  (format "\\(%s\\|%s\\|[;@&=+$,]\\)+"
	  bm-uri-unreserved-re bm-uri-escaped-re))

(defvar bm-uri-abs-path-re
  (format "\\(/%s\\)" bm-uri-path-segments-re))

(defvar bm-uri-rel-path-re
  (format "\\(%s%s?\\)"
	  bm-uri-rel-segment-re bm-uri-abs-path-re))

(defvar bm-uri-opaque-part-re
  (format "\\(%s%s*\\)" bm-uri-uric-no-slash-re bm-uri-uric-re))

(defvar bm-uri-path-re
  (format "\\(%s\\|%s\\)?" bm-uri-abs-path-re bm-uri-opaque-part-re))
	  
(defvar bm-uri-port-re
  "[0-9]*")

(defvar bm-uri-IPv4address-re
  "\\([0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\)")

(defvar bm-uri-toplabel-re
  "\\([a-zA-Z]\\([a-zA-Z0-9-]*[a-zA-Z0-9]\\)?\\)")

(defvar bm-uri-domainlabel-re
  "\\([a-zA-Z0-9][a-zA-Z0-9-]*[a-zA-Z0-9]\\)")

(defvar bm-uri-hostname-re
  (format "\\(\\(%s\\.\\)*%s\\.?\\)"
	  bm-uri-domainlabel-re bm-uri-toplabel-re))

(defvar bm-uri-host-re
  (format "\\(%s\\|%s\\)" bm-uri-hostname-re bm-uri-IPv4address-re))

(defvar bm-uri-hostport-re
  (format "\\(%s\\(:%s\\)?\\)" bm-uri-host-re bm-uri-port-re))

(defvar bm-uri-userinfo-re
  (format "\\(%s\\|%s\\|[;:&=+$,]\\)*"
	  bm-uri-unreserved-re bm-uri-escaped-re))

(defvar bm-uri-server-re
  (format "\\(\\(%s@\\)?%s\\)?"
	  bm-uri-userinfo-re bm-uri-hostport-re))

(defvar bm-uri-reg-name-re
  (format "\\(%s\\|%s\\|[$,;:@&=+]\\)+"
	  bm-uri-unreserved-re bm-uri-escaped-re))

(defvar bm-uri-authority-re
  (format "\\(%s\\|%s\\)"
	  bm-uri-server-re bm-uri-reg-name-re))

(defvar bm-uri-scheme-re
  "\\([a-zA-Z][a-zA-Z0-9+.-]*\\)")

(defvar bm-uri-net-path-re
  (format "\\(//%s%s?\\)"
	  bm-uri-authority-re bm-uri-abs-path-re))

(defvar bm-uri-hier-part-re
  (format "\\(\\(%s\\|%s\\)\\(\\?%s\\)?\\)"
	  bm-uri-net-path-re bm-uri-abs-path-re bm-uri-query-re))

(defvar bm-uri-relativeURI-re
  (format "\\(\\(%s\\|%s\\|%s\\)\\(\\?%s\\)?\\)"
	  bm-uri-net-path-re
	  bm-uri-abs-path-re
	  bm-uri-rel-path-re
	  bm-uri-query-re))

(defvar bm-uri-absoluteURI-re
  (format "\\(%s:\\(%s\\|%s\\)\\)"
	  bm-uri-scheme-re bm-uri-hier-part-re bm-uri-opaque-part-re))

(defvar bm-uri-URI-reference-re
  (format "\\(\\(%s\\|%s\\)?\\(#%s\\)?\\)"
	  bm-uri-absoluteURI-re bm-uri-relativeURI-re bm-uri-fragment-re))


(defvar bm-uri-abs-URI-ref-re
  (format "%s\\(#%s\\)?" bm-uri-absoluteURI-re bm-uri-fragment-re))

;;-----------------------------------------------
;; Popular URI schemes (for fast searching)
;;-----------------------------------------------
(defvar bm-uri-known-scheme-re
  "https?://\\|ftp://\\|gopher://\\|telnet://\\|wais://\\|file:/\\|s?news:\\|mailto:"
  "Regexp for URL scheme.")


;;-----------------------------------------------
;; Break URI into components
;;-----------------------------------------------
(defvar bm-uri-components-re
  (concat "^\\(\\([^:/?#]+\\):\\)?"
	  "\\(//\\([^/?#]*\\)\\)?"
	  "\\([^?#]*\\)\\(\\?\\([^#]*\\)\\)?"
	  "\\(#\\(.*\\)\\)?")
  "Regexp to break URI into components.

   Generic URI syntax consists of a sequence of four main components:
      <scheme>://<authority><path>?<query>

e.g. (setq uri \"http://www.ics.uci.edu/pub/ietf/uri/#Related\")
     (string-match bm-uri-components-re uri)

     (match-string 1 uri) => http:
     (match-string 2 uri) => http                 ; scheme
     (match-string 3 uri) => //www.ics.uci.edu
     (match-string 4 uri) => www.ics.uci.edu      ; authority
     (match-string 5 uri) => /pub/ietf/uri/       ; path
     (match-string 6 uri) => nil
     (match-string 7 uri) => nil                  ; query
     (match-string 8 uri) => #Related
     (match-string 9 uri) => Related              ; fragment
")

(defun a ()
  (interactive)
  (re-search-forward (format "%s\\(#%s\\)?"
			     bm-uri-absoluteURI-re bm-uri-fragment-re))
  (set-mark (match-beginning 0)))

(provide 'bm-uri)

;;; bm-uri.el ends here
