;;; bingdings-tab.el --- tab configuration -*- lexical-binding: t -*-
;;; Commentary:

;;; @see https://emacs.stackexchange.com/questions/7908/how-to-make-yasnippet-and-company-work-nicer/
;;; makes <tab> do the right thing most of the time.Pressing tab will
;;; 1. Indent the current line,
;;; 2. If there is a yasnippet to expand, expand it, even if
;;; this means aborting a company completion (no abbreviation support yet)
;;; 3. If a company completion is ongoing, complete with the selected item
;;; 4. Otherwise try to use company to start autocomplete
;;; 5. If there is nothing to autocomplete and we're in a yasnippet placeholder,
;;; skip to the next placeholder.

;;; Tips:
;;; 1. if there is an opportunity to autocomplete and you are
;;; currently editing in a snippet placeholder, the situation is ambigous.
;;; As a compromise, I bound C-<tab> to skip to the next placeholder directly.
;;;
;;; The fact that the snippet's name does not appear in the company menu and
;;; the existence of a snippet silently modifies the behaviour of the tab key is
;;; not particularly nice, unfortunately... Although at least it is possible to
;;; type <return> instead to get the completion instead of the snippet.

;;; code:

(after [company yasnippet]
  (defun check-expansion ()
    (save-excursion
      (if (looking-at "\\_>") t
        (backward-char 1)
        (if (looking-at "\\.") t
    (backward-char 1)
    (if (looking-at "->") t nil)))))

  (defun do-yas-expand ()
    (let ((yas/fallback-behavior 'return-nil))
      (yas/expand)))

  (defun tab-indent-or-complete ()
    (interactive)
    (cond
    ((minibufferp)
      (minibuffer-complete))
    (t
      (indent-for-tab-command)
      (if (or (not yas/minor-mode)
        (null (do-yas-expand)))
    (if (check-expansion)
        (progn
          (company-manual-begin)
          (if (null company-candidates)
        (progn
          (company-abort)
          (indent-for-tab-command)))))))))

  (defun tab-complete-or-next-field ()
    (interactive)
    (if (or (not yas/minor-mode)
      (null (do-yas-expand)))
        (if company-candidates
      (company-complete-selection)
    (if (check-expansion)
        (progn
          (company-manual-begin)
          (if (null company-candidates)
        (progn
          (company-abort)
          (yas-next-field))))
      (yas-next-field)))))

  (defun expand-snippet-or-complete-selection ()
    (interactive)
    (if (or (not yas/minor-mode)
      (null (do-yas-expand))
      (company-abort))
        (company-complete-selection)))

  (defun abort-company-or-yas ()
    (interactive)
    (if (null company-candidates)
        (yas-abort-snippet)
      (company-abort)))

  (global-set-key [tab] 'tab-indent-or-complete)
  (global-set-key (kbd "TAB") 'tab-indent-or-complete)
  (global-set-key [(control return)] 'company-complete-common)

  (define-key company-active-map [tab] 'expand-snippet-or-complete-selection)
  (define-key company-active-map (kbd "TAB") 'expand-snippet-or-complete-selection)

  (define-key yas-minor-mode-map [tab] nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)

  (define-key yas-keymap [tab] 'tab-complete-or-next-field)
  (define-key yas-keymap (kbd "TAB") 'tab-complete-or-next-field)
  (define-key yas-keymap [(control tab)] 'yas-next-field)
  (define-key yas-keymap (kbd "C-g") 'abort-company-or-yas))

(provide 'bindings-tab)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; bindings-tab.el ends here
