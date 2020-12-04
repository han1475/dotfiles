;;; config-linum-mode.el --- linum-mode configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; code:

(setq display-line-numbers-width 2)
(setq display-line-numbers-grow-only t)
(set-face-background 'line-number "#BCBCBC")
(set-face-foreground 'line-number-current-line "#002b36")

;; http://stackoverflow.com/questions/3875213/turning-on-linum-mode-when-in-python-c-mode
(setq linum-mode-inhibit-modes-list '(eshell-mode
				      shell-mode
				      profiler-report-mode
				      ffip-diff-mode
				      dictionary-mode
				      erc-mode
				      dired-mode
				      help-mode
				      text-mode
				      fundamental-mode
				      jabber-roster-mode
				      jabber-chat-mode
				      inferior-js-mode
				      inferior-python-mode
				      ivy-occur-grep-mode ; better performance
				      ivy-occur-mode ; better performance
				      twittering-mode
				      compilation-mode
				      weibo-timeline-mode
				      woman-mode
				      Info-mode
				      calc-mode
				      calc-trail-mode
				      comint-mode
				      gnus-group-mode
				      gud-mode
				      vc-git-log-edit-mode
				      log-edit-mode
				      term-mode
				      w3m-mode
				      speedbar-mode
				      gnus-summary-mode
				      gnus-article-mode
				      calendar-mode))

(defun display-line-numbers-mode-hook-setup ()
  (setq display-line-numbers (if (or (memq major-mode linum-mode-inhibit-modes-list)
				     ;; don't show line number for certain file extensions
				     (should-use-minimum-resource))
				 nil
			       t)))
(add-hook 'display-line-numbers-mode-hook 'display-line-numbers-mode-hook-setup)
(global-display-line-numbers-mode t)

(provide 'config-linum-mode)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; config-linum-mode.el ends here
