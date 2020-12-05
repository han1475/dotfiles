;;; config-utils.el --- Elisp helper functions and commands -*- lexical-binding: t -*-
;;; Commentary:
;;; code:

(defun should-use-minimum-resource ()
  (and buffer-file-name
       (string-match-p "\.\\(mock\\|min\\)\.js" buffer-file-name)))

;; from https://github.com/bling/dotemacs/blob/master/core/core-boot.el#L53-L74
;; see https://github.com/bling/dotemacs#simple-building-block-2
(defmacro after (feature &rest body)
  "Executes BODY after FEATURE has been loaded.
FEATURE may be any one of:
    'evil            => (with-eval-after-load 'evil BODY)
    \"evil-autoloads\" => (with-eval-after-load \"evil-autolaods\" BODY)
    [evil cider]     => (with-eval-after-load 'evil
                          (with-eval-after-load 'cider
                            BODY))
"
  (declare (indent 1))
  (cond
   ((vectorp feature)
    (let ((prog (macroexp-progn body)))
      (cl-loop for f across feature
               do
               (progn
                 (setq prog (append `(',f) `(,prog)))
                 (setq prog (append '(with-eval-after-load) prog))))
      prog))
   (t
    `(with-eval-after-load ,feature ,@body))))
       

(provide 'config-utils)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; config-utils.el ends here
