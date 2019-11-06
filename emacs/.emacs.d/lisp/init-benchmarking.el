;;; init-benchmarking.el --- Measure startup and require times -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(defun han1475/time-subtract-millis (b a)
  (* 1000.0 (float-time (time-subtract b a))))


(defvar han1475/require-times nil
  "A list of (FEATURE LOAD-START-TIME LOAD-DURATION).
LOAD-DURATION is the time taken in milliseconds to load FEATURE.")

(defun han1475/require-times-wrapper (orig feature &rest args)
  "Note in `han1475/require-times' the time taken to require each feature."
  (let* ((already-loaded (memq feature features))
         (require-start-time (and (not already-loaded) (current-time))))
    (prog1
        (apply orig feature args)
      (when (and (not already-loaded) (memq feature features))
        (let ((time (han1475/time-subtract-millis (current-time) require-start-time)))
          (add-to-list 'han1475/require-times
                       (list feature require-start-time time)
                       t))))))

(advice-add 'require :around 'han1475/require-times-wrapper)


(define-derived-mode han1475/require-times-mode tabulated-list-mode "Require-Times"
  "Show times taken to `require' packages."
  (setq tabulated-list-format
        [("Start time (ms)" 20 han1475/require-times-sort-by-start-time-pred)
         ("Feature" 30 t)
         ("Time (ms)" 12 han1475/require-times-sort-by-load-time-pred)])
  (setq tabulated-list-sort-key (cons "Start time (ms)" nil))
  ;; (setq tabulated-list-padding 2)
  (setq tabulated-list-entries #'han1475/require-times-tabulated-list-entries)
  (tabulated-list-init-header)
  (when (fboundp 'tablist-minor-mode)
    (tablist-minor-mode)))

(defun han1475/require-times-sort-by-start-time-pred (entry1 entry2)
  (< (string-to-number (elt (nth 1 entry1) 0))
     (string-to-number (elt (nth 1 entry2) 0))))

(defun han1475/require-times-sort-by-load-time-pred (entry1 entry2)
  (> (string-to-number (elt (nth 1 entry1) 2))
     (string-to-number (elt (nth 1 entry2) 2))))

(defun han1475/require-times-tabulated-list-entries ()
  (cl-loop for (feature start-time millis) in han1475/require-times
           with order = 0
           do (incf order)
           collect (list order
                         (vector
                          (format "%.3f" (han1475/time-subtract-millis start-time before-init-time))
                          (symbol-name feature)
                          (format "%.3f" millis)))))

(defun han1475/require-times ()
  "Show a tabular view of how long various libraries took to load."
  (interactive)
  (with-current-buffer (get-buffer-create "*Require Times*")
    (han1475/require-times-mode)
    (tabulated-list-revert)
    (display-buffer (current-buffer))))




(defun han1475/show-init-time ()
  (message "init completed in %.2fms"
           (han1475/time-subtract-millis after-init-time before-init-time)))

(add-hook 'after-init-hook 'han1475/show-init-time)


(provide 'init-benchmarking)
;;; init-benchmarking.el ends here
