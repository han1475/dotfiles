;;; config-misc.el --- misc configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; code:

;; Electric Pair
(setq electric-pair-pairs '(
			    (?\" . ?\")
			    (?\` . ?\`)
			    (?\( . ?\))
			    (?\{ . ?\})
			    (?\~ . ?\~)
			    ))
(add-hook 'text-mode-hook #'electric-pair-mode)
(add-hook 'prog-mode-hook #'electric-pair-mode)

;; Eighty Column Rule
(setq-default fill-column 80)
(add-hook 'text-mode-hook #'display-fill-column-indicator-mode)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

(provide 'config-misc)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; config-misc.el ends here
