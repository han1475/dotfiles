;;; init-git.el --- git configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; code:

(require-package 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

(provide 'init-git)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-git.el ends here
