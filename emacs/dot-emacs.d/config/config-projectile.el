;;; config-projectile.el --- projectile configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; code:

(require-package 'projectile)
(setq projectile-indexing-method 'alien)
(setq projectile-enable-caching t)
(setq projectile-completion-system 'ivy)
(add-hook 'after-init-hook 'projectile-mode)

(provide 'config-projectile)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; config-projectile.el ends here
