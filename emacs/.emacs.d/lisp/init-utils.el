;;; init-utils.el --- Elisp helper functions and commands -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun should-use-minimum-resource ()
  (and buffer-file-name
       (string-match-p "\.\\(mock\\|min\\)\.js" buffer-file-name)))

(provide 'init-utils)
;;; init-utils.el ends here
