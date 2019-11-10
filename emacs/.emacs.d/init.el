;;;init.el --- Load the full configuration -*- coding: utf-8; lexical-binding: t; -*-
;; Produce backtraces when errors occur
(setq debug-on-error t)


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
(require 'init-ox-hugo)
(require 'init-yasnippet)



;;; Local Variables:
;;; no-byte-compile: t
;;; init.el ends here
