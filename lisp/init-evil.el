;;; init-evil.el --- evil configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; code:

;; Dependencies:
;; Evil requires undo-tree.el in the load-path for
;; linear undo and undo branches.
(require-package 'undo-tree)
;; For the motions g; g, and for the last-change-register .,
;; Evil requires the goto-chg.el package,
;; which provides the functions goto-last-change and goto-last-change-reverse.
(require-package 'goto-chg)

(require-package 'evil)

;; Enable evil-mode
;; evil must be require explicitly, the autoload seems to not
;; work properly sometimes.
(require 'evil)
(evil-mode 1)

(provide 'init-evil)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-evil.el ends here
