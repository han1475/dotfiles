;;; hydras.el ---  hydra bindings configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; code:

;;; flycheck used by default for errors
(defvar /hydras/errors/flycheck nil)
(defun /hydras/errors/target-list ()
  "Return target list."
  (if /hydras/errors/flycheck
      'flycheck
    'emacs))
(defhydra /hydras/errors (:hint nil)
    "
   errors:  navigation                 flycheck
            -----------------------    ---------------
            _j_ → next error             _l_ → list errors
            _k_ → previous error         _?_ → describe checker
            _t_ → toggle list (%(/hydras/errors/target-list))
"
    ("j" (if /hydras/errors/flycheck
	     (call-interactively #'flycheck-next-error)
	   (call-interactively #'next-error)))
    ("k" (if /hydras/errors/flycheck
	     (call-interactively #'flycheck-previous-error)
	   (call-interactively #'previous-error)))
    ("t" (setq /hydras/errors/flycheck (not /hydras/errors/flycheck)))
    ("?" flycheck-describe-checker)
      ("l" flycheck-list-errors :exit t))

(defhydra /hydras/quit (:hint nil :exit t)
    "
   quit:  _q_ → quit    _r_ → restart
"
    ("q" save-buffers-kill-terminal)
    ("r" (restart-emacs '("--debug-init"))))

(provide 'hydras)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; hydras.el ends here
