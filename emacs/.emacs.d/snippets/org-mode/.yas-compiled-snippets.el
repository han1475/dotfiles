;;; Compiled snippets and support files for `org-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'org-mode
		     '(("h" "#+HUGO_BASE_DIR: ../\n#+TITLE: $1\n#+DATE: `(format-time-string \"%Y-%m-%d\")`\n#+HUGO_AUTO_SET_LASTMOD: t\n#+HUGO_TAGS: $2\n#+HUGO_CATEGORIES: $3\n#+HUGO_DRAFT: false\n<!--more-->\n\n$0\n" "hugo" nil nil nil "/home/han/.emacs.d/snippets/org-mode/hugo" nil nil)))


;;; Do not edit! File generated at Wed Nov  6 23:29:54 2019
