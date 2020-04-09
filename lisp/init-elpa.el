;;; init-elpa.el --- elpa configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; code:

;; no need to activate all the packages so early
(setq package-enable-at-startup nil)
(package-initialize)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https:/stable.melpa.org/packages/")))
(provide 'init-elpa)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-elpa.el ends here
