;;; init-yasnippet.el --- yasnippet configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; code:

(require-package 'yasnippet)
;; my private snippets, should be placed before enabling yasnippet
(setq my-yasnippets (expand-file-name "~/.emacs.d/yasnippet"))

(defun yasnippet-generic-setup-for-mode-hook ()
  (unless (is-buffer-file-temp) (yas-minor-mode 1)))

(add-hook 'prog-mode-hook 'yasnippet-generic-setup-for-mode-hook)
(add-hook 'text-mode-hook 'yasnippet-generic-setup-for-mode-hook)

(with-eval-after-load "yasnippet"
  ;; http://stackoverflow.com/questions/7619640/emacs-latex-yasnippet-why-are-newlines-inserted-after-a-snippet
  (setq-default mode-require-final-newline nil)
  ;; (message "yas-snippet-dirs=%s" (mapconcat 'identity yas-snippet-dirs ":"))

  ;; Use `yas-dropdown-prompt' if possible. It requires `dropdown-list'.
  (setq yas-prompt-functions '(yas-dropdown-prompt
                               yas-ido-prompt
                               yas-completing-prompt))

  ;; use `yas-completing-prompt' when ONLY when `M-x yas-insert-snippet'
  ;; thanks to capitaomorte for providing the trick.
  (defadvice yas-insert-snippet (around use-completing-prompt activate)
    "Use `yas-completing-prompt' for `yas-prompt-functions' but only here..."
    (let* ((yas-prompt-functions '(yas-completing-prompt)))
      ad-do-it))

  (when (and  (file-exists-p my-yasnippets)
              (not (member my-yasnippets yas-snippet-dirs)))
    (add-to-list 'yas-snippet-dirs my-yasnippets))

  (yas-reload-all))

(provide 'init-yasnippet)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-yasnippet.el ends here
