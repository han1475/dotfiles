;;; config-eighty-column.el --- eighty column rule -*- lexical-binding: t -*-
;;; Commentary:
;;; code:

;; Default setting
(setq-default fill-column 80)
(add-hook 'text-mode-hook #'display-fill-column-indicator-mode)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

(provide 'config-eighty-column)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; config-eighty-column.el ends here
