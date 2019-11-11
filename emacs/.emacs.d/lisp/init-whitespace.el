;;; init-whitespace.el --- EightyColumnRule with whitespace -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'whitespace)
;;(see https://www.emacswiki.org/emacs/EightyColumnRule)
(setq whitespace-line-column 80);;default 80

(setq whitespace-style
      '(face
        ;; trailing blanks
        trailing
        ;; empty lines at beginning and/or end of buffer
        ;; empty
        ;; line is longer `whitespace-line-column'
        lines-tail
        ;; tab or space at the beginning of the line according to
        ;; `indent-tabs-mode'
        ;indentation
        ;; show tab as » (see `whitespace-display-mappings')
        tab-mark))
(global-whitespace-mode t)

(provide 'init-whitespace)
;;; init-whitespace.el ends here