;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration.

;;; code

(let  ((normal-gc-cons-threshold (* 20 1024 1024))
       (init-gc-cons-threshold (* 128 1024 1024)))
  
  ;;Adjust garbage collection thresholds during startup, and thereafter
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold)))
  
  ;; Measure startup time and garbage collections
  (add-hook 'emacs-startup-hook
      	    (lambda ()
	      (message "Emacs ready in %s with %d garbage collections."
       		       (format "%.2f seconds"
	    		       (float-time
	    			(time-subtract after-init-time before-init-time)))
	    	       gcs-done))))

(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
