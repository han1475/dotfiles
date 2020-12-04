;;; early-init.el --- Emacs 27+ pre-initialisation configuration -*- lexical-binding: t -*-
;;; Commentary:

;; Emacs 27+ loads this file before (normally) calling
;; `package-initialize'.  We use this file to suppress that automatic
;; behaviour so that startup is consistent across Emacs versions.

;;; Code:

;; no need to activate all the packages so early
(setq package-enable-at-startup nil)

;; So we can detect this having been loaded
(provide 'early-init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; early-init.el ends here
