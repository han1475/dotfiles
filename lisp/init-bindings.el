;;; init-bindings.el --- bindings configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; code:

;; Displays the key bindings following your currently entered incomplete command
(require-package 'which-key)
(setq which-key-idle-delay 0.2)
(setq which-key-min-display-lines 3)
(which-key-mode)

(require-package 'hydra)

(provide 'init-bindings)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-bindings.el ends here
