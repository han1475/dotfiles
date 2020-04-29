;;; config-ox-hugo.el --- ox-hugo configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; code:

(require-package 'ox-hugo)
(after 'ox
    (require 'ox-hugo))
(setq org-hugo-default-section-directory "post")

(provide 'config-ox-hugo)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; config-ox-hugo.el ends here
