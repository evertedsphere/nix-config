;;; config/agenda.el -*- lexical-binding: t; -*-

(use-package! vulpea
  :hook ((org-roam-db-autosync-mode . vulpea-db-autosync-enable)))
(use-package! vulpea-buffer)

;; strip the ugly roam timestamp from the agenda
(setq org-agenda-prefix-format
      '((agenda . " %i %-20(local/agenda-category)%?-20t% s")
        (todo . " %i %-20(local/agenda-category) ")
        (tags . " %i %-20(local/agenda-category) ")
        (search . " %i %-20(local/agenda-category) ")))

;; https://d12frosted.io/posts/2020-06-24-task-management-with-roam-vol2.html
(defun local/agenda-category ()
  "Get category of item at point for agenda.

Category is defined by one of the following items:

- CATEGORY property
- TITLE keyword
- TITLE property
- filename without directory and extension

Usage example:

  (setq org-agenda-prefix-format
        \\='((agenda . \" %(vulpea-agenda-category) %?-12t %12s\")))

Refer to `org-agenda-prefix-format' for more information."
  (let* ((file-name (when buffer-file-name
                      (file-name-sans-extension
                       (file-name-nondirectory buffer-file-name))))
         (title (vulpea-buffer-title-get))
         (category (org-get-category)))
    (or (if (and
             title
             (string-equal category file-name))
            title
          ;; there is no title, or the category is not the default one
          category)
        "")))
