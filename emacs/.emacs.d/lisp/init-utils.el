;;; init-utils.el --- Elisp helper functions and commands -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

(if (fboundp 'with-eval-after-load)
    (defalias 'after-load 'with-eval-after-load)
  (defmacro after-load (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))
       
(defun should-use-minimum-resource ()
  (and buffer-file-name
       (string-match-p "\.\\(mock\\|min\\)\.js" buffer-file-name)))

(provide 'init-utils)
;;; init-utils.el ends here
