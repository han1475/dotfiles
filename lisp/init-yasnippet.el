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

(provide 'init-yasnippet)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-yasnippet.el ends here
