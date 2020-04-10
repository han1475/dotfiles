;;; init-elpa.el --- elpa configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; code:

;; no need to activate all the packages so early
(setq package-enable-at-startup nil)
(package-initialize)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https:/stable.melpa.org/packages/")))

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

(provide 'init-elpa)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-elpa.el ends here
