;;; config-git.el --- git configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; code:

(require-package 'magit)
(require-package 'git-gutter)

(setq vc-handled-backends '(Git))

;; When you open a file which is a symlink to a file under version control,
;; automatically switch and open the actual file.  
(setq vc-follow-symlinks t)

;; @see https://www.reddit.com/r/emacs/comments/4c0mi3/the_biggest_performance_improvement_to_emacs_ive/
;; open files faster but you can't check if file is version
;; controlled. other VCS functionality still works.
(remove-hook 'find-file-hooks 'vc-find-file-hook)

(defun git-gutter-reset-to-head-parent()
  "Reset  gutter to HEAD^.  Support Subversion and Git."
  (interactive)
  (let* (parent (filename (buffer-file-name)))
    (
      (setq parent (if filename (concat (shell-command-to-string (concat "git --no-pager log --oneline -n1 --pretty=\"format:%H\" " filename)) "^") "HEAD^")))
    (git-gutter:set-start-revision parent)
    (message "git-gutter:set-start-revision HEAD^")))

(defun my-git-commit-id ()
  "Select commit id from current branch."
  (let* ((git-cmd "git --no-pager log --date=short --pretty=format:'%h|%ad|%s|%an'")
	 (collection (nonempty-lines (shell-command-to-string git-cmd)))
	 (item (ffip-completing-read "git log:" collection)))
    (when item
      (car (split-string item "|" t)))))

(defun my-git-show-commit-internal ()
  "Show git commit"
  (let* ((id (my-git-commit-id)))
    (when id
      (shell-command-to-string (format "git show %s" id)))))

(defun my-git-show-commit ()
  "Show commit using ffip."
  (interactive)
  (let* ((ffip-diff-backends '(("Show git commit" . my-git-show-commit-internal))))
    (ffip-show-diff 0)))

(defun git-gutter-toggle ()
  "Toggle git gutter."
  (interactive)
  (git-gutter-mode -1)
  ;; git-gutter-fringe doesn't seem to
  ;; clear the markup right away
  (sit-for 0.1)
  (git-gutter:clear))

(defun git-gutter-reset-to-default ()
    "Restore git gutter to its original status.
Show the diff between current working code and git head."
    (interactive)
    (git-gutter:set-start-revision nil)
    (message "git-gutter reset"))

(global-git-gutter-mode t)

;; @see https://emacs.stackexchange.com/questions/27946/yasnippets-wont-expand-in-git-commit-mode
;; Yasnippets won't expand in magit-commit(git-commit-mode).
;; git-commit-mode is a minor mode and it's major mode is text-mode, but yasnippet expend
;; only in major mode and in text-mode <TAB> is
;; just <TAB> key mapping, so yanippet don't work.
(setq git-commit-major-mode 'org-mode)

(global-set-key (kbd "C-x C-g") 'git-gutter)
(global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)
;; Stage current hunk
(global-set-key (kbd "C-x v s") 'git-gutter:stage-hunk)
;; Revert current hunk
(global-set-key (kbd "C-x v r") 'git-gutter:revert-hunk)

(provide 'config-git)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; config-git.el ends here
