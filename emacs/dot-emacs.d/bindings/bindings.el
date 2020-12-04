;;; bindings.el --- bindings configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; code:

;; Displays the key bindings following your currently entered incomplete command
(require-package 'which-key)
(setq which-key-idle-delay 0.2)
(setq which-key-min-display-lines 3)
(which-key-mode)

;; Use a fancy separator between Dedicated window for
;; Hydra hints and the Echo Area
(setq lv-use-separator t)
(require-package 'hydra)
(autoload 'hydra-default-pre "hydra")

;; bindings macro
(defmacro /bindings/define-prefix-keys (keymap prefix &rest body)
  (declare (indent defun))
  `(progn
     ,@(cl-loop for binding in body
		collect
		`(let ((seq ,(car binding))
		       (func ,(cadr binding))
		       (desc ,(caddr binding)))
		   (define-key ,keymap (kbd seq) func)
		   (when desc
		     (which-key-add-key-based-replacements
		       (if ,prefix
			   (concat ,prefix " " seq)
			 seq)
		       desc))))))

(defmacro /bindings/define-keys (keymap &rest body)
  (declare (indent defun))
  `(/bindings/define-prefix-keys ,keymap nil ,@body))

(defmacro /bindings/define-key (keymap sequence binding &optional description)
  (declare (indent defun))
  `(/bindings/define-prefix-keys ,keymap nil
				      (,sequence ,binding ,description)))

(provide 'bindings)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; bindings.el ends here
