;;; bm-lookup-wn.el --- 

;;; Code:

(require 'lookup-package)

(defconst wn-reference-pattern
  '("{\\([^}]+\\)}" 1 (lookup-oneline-string (match-string 1))
    lookup-dynamic-code-search))

(defconst wn-arrange-functions
  '(lookup-arrange-references
    wn-arrange-headings
    ;lookup-arrange-fill-paragraphs
))

(defconst wn-adjust-functions
  '(lookup-adjust-check-references
    lookup-adjust-goto-min))

(defun wn-arrange-headings (entry)
  (if (looking-at "[^/\n]+")
      (lookup-make-region-heading (match-beginning 0) (match-end 0) 1)))

(setq lookup-package-dictionary-options-alist
      `(("wn"
	     (:title . "Word Net")
	     (:reference-pattern . ,wn-reference-pattern)
	     (:arranges . ,wn-arrange-functions)
	     (:adjusts . ,wn-adjust-functions)
)))

;;; bm-lookup-wn.el ends here
