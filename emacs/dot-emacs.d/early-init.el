;;; early-init.el --- Emacs 27+ pre-initialisation configuration -*- lexical-binding: t -*-
;;; Commentary:

;; Emacs 27+ loads this file before (normally) calling
;; `package-initialize'.  We use this file to suppress that automatic
;; behaviour so that startup is consistent across Emacs versions.

;;; Code:

;; no need to activate all the packages so early
;; So we can detect this having been loaded
(setq package-enable-at-startup nil)

;; Frame Parameter
(setq default-frame-alist
       '((height . 30)
         (width . 100)
         (left . (- 546))
         (top .  (- 270))
         (font . "DejaVu Sans Mono-12")
         (cursor-type . bar)
         (vertical-scroll-bars . nil)
         (horizontal-scroll-bars . nil)
         (tool-bar-lines . 0)
         (menu-bar-lines . 0)))
         
(provide 'early-init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; early-init.el ends here
