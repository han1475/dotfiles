;;; init-bindings.el --- bindings configuration -*- lexical-binding: t -*-
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

(provide 'init-bindings)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-bindings.el ends here
