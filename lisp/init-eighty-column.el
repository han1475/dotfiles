;;; init-eighty-column.el --- eighty column rule -*- lexical-binding: t -*-
;;; Commentary:
;;; code:

(setq-default whitespace-line-column 80
	      whitespace-style       '(face lines-tail))
(add-hook 'prog-mode-hook #'whitespace-mode)

(provide 'init-eighty-column)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-eighty-column.el ends here
