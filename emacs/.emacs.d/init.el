;;;init.el --- Load the full configuration -*- coding: utf-8; lexical-binding: t; -*-
;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;;(setq debug-on-error t)


(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))


(require 'init-start-up-quickly)
;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(require 'init-utils)
(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
;; Calls (package-initialize)
(require 'init-elpa)      ;; Machinery for installing required packages

;; Extra packages which don't require any configuration


;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------
(require 'init-linum-mode)
(require 'init-ox-hugo)
(require 'init-yasnippet)
(require 'init-ivy)
(require 'init-company)
(require 'init-whitespace)


;;; Local Variables:
;;; no-byte-compile: t
;;; init.el ends here
