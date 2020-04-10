;;; init-ox-hugo.el --- ox-hugo configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; code:

(require-package 'ox-hugo)
(with-eval-after-load 'ox
    (require 'ox-hugo))
(setq org-hugo-default-section-directory "post")

(provide 'init-ox-hugo)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-ox-hugo.el ends here
