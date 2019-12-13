;;; init-evil.el --- Suport for evil -*- lexical-binding: t -*-
;;; Commentary:
;;; Code

;;goto-chg
(require-package 'goto-chg)
(global-set-key [(control ?.)] 'goto-last-change)
(global-set-key [(control ?,)] 'goto-last-change-reverse)

;;undo-tree
(require-package 'undo-tree)

(require-package 'evil)
;; enable evil-mode
(evil-mode 1)

(provide 'init-evil)
;;; init-evil.el ends here