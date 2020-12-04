;;; config-flycheck.el --- flycheck configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; code:

(require-package 'flycheck)
(require-package 'flycheck-color-mode-line)

(setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
(add-hook 'after-init-hook 'global-flycheck-mode)
(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)

(provide 'config-flycheck)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; config-flycheck.el ends here
