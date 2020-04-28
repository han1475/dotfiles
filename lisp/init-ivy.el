;;; init-ivy.el --- ivy configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; code:

(require-package 'ivy)
(require-package 'counsel)
(require-package 'swiper)

(setq ivy-use-virtual-buffers t)
;; When runing ivy-switch-buffers, display full path of bookmarks and recent files.
(setq ivy-virtual-abbreviate 'full)
(setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
(setq ivy-height 16)
(setq ivy-display-style 'fancy)
(setq ivy-count-format "[%d/%d] ")
(setq ivy-initial-inputs-alist nil)

(ivy-mode t)

(provide 'init-ivy)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-ivy.el ends here
