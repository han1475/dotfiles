;;; init-auto-save.el --- Suport for auto-save -*- lexical-binding: t -*-
;;; Commentary:
;;; Code

(local-require 'auto-save)
(auto-save-enable)
(setq auto-save-silent t)   ; quietly save
(setq auto-save-delete-trailing-whitespace t)  ; automatically delete spaces at the end of the line when saving
(auto-save-idle 2) ; auto save every two seconds

(provide 'init-auto-save)
;;; init-auto-save.el ends here