;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration.

;;; code

(let  ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024))
	  (core-directory (concat user-emacs-directory "core/"))
      (bindings-directory (concat user-emacs-directory "bindings/"))
      (config-directory (concat user-emacs-directory "config/")))

      ;; Measure startup time and garbage collections
      (add-hook 'emacs-startup-hook
      	  (lambda ()
	         (message "Emacs ready in %s with %d garbage collections."
       		     (format "%.2f seconds"
	    		     (float-time
	    		      (time-subtract after-init-time before-init-time)))
	    	     gcs-done)))

      ;;Adjust garbage collection thresholds during startup, and thereafter
      (setq gc-cons-threshold init-gc-cons-threshold)
      (add-hook 'emacs-startup-hook
	                    (lambda () (setq gc-cons-threshold normal-gc-cons-threshold)))

      ;; Disable the Menu bar when in text-only terminals
      (unless (display-graphic-p) (menu-bar-mode -1))


	  (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")))
	  (package-initialize)

      (load (concat core-directory "core-boot"))

      (setq custom-file (concat user-emacs-directory "custom.el"))
      (when (file-exists-p custom-file)
      (load custom-file))
	  (cl-loop for file in (append (reverse (directory-files-recursively config-directory "\\.el$"))
                               (reverse (directory-files-recursively bindings-directory "\\.el$")))
           do (condition-case ex
                  (load (file-name-sans-extension file))
                ('error (with-current-buffer "*scratch*"
                          (insert (format "[INIT ERROR]\n%s\n%s\n\n" file ex)))))))

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
