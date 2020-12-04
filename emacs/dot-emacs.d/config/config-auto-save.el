;;; config-auto-save.el --- auto-save configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; code:

(defcustom auto-save-exclude '("\\.avi"
                               "\\.mpeg"
                               "\\.3gp"
                               "\\.mp4"
                               "\\.mp3"
                               "\\.mkv"
                               "\\.rm"
                               "\\.rmvb"
                               "\\.pdf"
                               "\\.jpg"
                               "\\.jpeg"
                               "\\.png"
                               "\\.gif"
                               "\\.gz"
                               "\\.svg"
                               "\\.ico"
                               "\\.gpg"
                               "archive-contents")
  "List of regexps and predicates for filenames excluded from the auto save list.
When a filename matches any of the regexps or satisfies any of the
predicates it is excluded from the auto save list.
A predicate is a function that is passed a filename to check and that
must return non-nil to exclude it.")

(add-to-list 'auto-save-exclude 'file-too-big-p t)

(require 'auto-save)
(auto-save-enable)
;; quietly save
(setq auto-save-silent t)
;; automatically delete spaces at the end of the line when saving
(setq auto-save-delete-trailing-whitespace t)
;; auto save every 2 seconds
(setq auto-save-idle 2)

(provide 'config-auto-save)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; config-auto-save.el ends here
