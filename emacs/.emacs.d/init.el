;;;init.el --- Load the full configuration -*- coding: utf-8; lexical-binding: t; -*-
;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;;(setq debug-on-error t)


(push (expand-file-name "lisp" user-emacs-directory) load-path)

;;----------------------------------------------------------------------------
;; Load configs for startup quickly
;;----------------------------------------------------------------------------
;;; Adjust garbage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; Measure startup time and garbage collections
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))


;; Unset file-name-handler-alist temporarily.
(defvar han1475/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq file-name-handler-alist han1475/file-name-handler-alist)))

;; when startup-now is true only load basic and text configuration.
(defun require-init (pkg &optional maybe-disabled)
  "Load PKG if MAYBE-DISABLED is nil or it's nil but start up in normal slowly."
  (when (or (not maybe-disabled) (not (boundp 'startup-now)))
    (load (file-truename (format "~/.emacs.d/lisp/%s" pkg)) t t)))


;; *Message* buffer should be writable in 24.4+
(defadvice switch-to-buffer (after switch-to-buffer-after-hack activate)
  (if (string= "*Messages*" (buffer-name))
      (read-only-mode -1)))

;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(require-init 'init-utils)
(require-init 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
;; Calls (package-initialize)
(require-init 'init-elpa)      ;; Machinery for installing required packages

;; Extra packages which don't require any configuration


;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------
(require-init 'init-linum-mode)
(require-init 'init-ox-hugo t)
(require-init 'init-yasnippet)
(require-init 'init-ivy)
(require-init'init-company t)
(require-init 'init-whitespace)


;;; Local Variables:
;;; no-byte-compile: t
;;; init.el ends here
