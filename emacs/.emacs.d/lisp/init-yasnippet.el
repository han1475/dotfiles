;;; init-yasnippet.el --- Support yasnippet -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require-package 'yasnippet)
(setq my-yasnippets (expand-file-name "~/.emacs.d/snippet"))
(yas-global-mode 1)

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
