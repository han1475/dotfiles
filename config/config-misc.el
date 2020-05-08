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

(provide 'config-misc)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; config-misc.el ends here
