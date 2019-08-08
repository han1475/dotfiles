;;; init-org-page.el --- Support org-page -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'ox-hugo)

;;； org-hugo capture
(with-eval-after-load 'org-capture
 (defun org-hugo-new-subtree-post-capture-template ()
     "Returns `org-capture' template string for new Hugo post.
 See `org-capture-templates' for more information."
     (let* (
            (date (format-time-string (org-time-stamp-format  :inactive) (org-current-time)))
            (title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
            (pfilename (read-from-minibuffer "file name: ")) ;Prompt to enter the post file name
            (fname (org-hugo-slug pfilename)))
       (mapconcat #'identity
                  `(
                    ,(concat "** TODO " title "     :标签:@分类:")
                    ":PROPERTIES:"
                    ,(concat ":EXPORT_FILE_NAME: " fname)
                    ,(concat ":EXPORT_DATE: " date) ;Enter current date and time
                    ":END:"
                    "%?\n")          ;Place the cursor here finally
                  "\n")))

  (add-to-list 'org-capture-templates
               '("b"                ;`org-capture' binding + h
                 "blog"
                 entry
                 ;; It is assumed that below file is present in `org-directory'
                 ;; and that it has a "Blog Ideas" heading. It can even be a
                 ;; symlink pointing to the actual location of all-posts.org!
                 (file+olp "~/blog-hugo/org/han.org" "blog")
                 (function org-hugo-new-subtree-post-capture-template))))
(setq org-hugo-default-section-directory "post")
(global-set-key (kbd "C-c c") 'org-capture)

(provide 'init-ox-hugo)
;;; init-ox-hugo.el ends here