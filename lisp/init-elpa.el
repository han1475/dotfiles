;;; init-elpa.el --- elpa configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; code:


(package-initialize)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")))

;; On-demand installation of packages
(defun require-package (package &optional min-version no-refresh)
  "Ask elpa to install given PACKAGE."
  (cond
   ((package-installed-p package min-version)
    t)
   ((or (assoc package package-archive-contents) no-refresh)
    (package-install package))
   (t
    (package-refresh-contents)
        (require-package package min-version t))))

;; Support elisp manually installed in the site-lisp dir
(defun local-require (pkg)
  (unless (featurep pkg)
    (load (expand-file-name (format "~/.emacs.d/site-lisp/%s/%s" pkg pkg)))))

(provide 'init-elpa)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-elpa.el ends here
