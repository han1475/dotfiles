;;; init-start-up-quickly.el --- Emacs start up quickly  -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:
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


;; Unset file-name-handler-alist temporarily
(defvar han1475/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq file-name-handler-alist han1475/file-name-handler-alist)))


(provide 'init-start-up-quickly)
;;; init-start-up-quickly.el ends here