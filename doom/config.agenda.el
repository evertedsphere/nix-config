;;; config/agenda.el -*- lexical-binding: t; -*-

(use-package! vulpea
  :hook ((org-roam-db-autosync-mode . vulpea-db-autosync-enable)))
(use-package! vulpea-buffer)

;; strip the ugly roam timestamp from the agenda
(setq org-agenda-prefix-format
      '((agenda . " %i %-40(local/agenda-prefix)%?-20t% s")
        (todo . " %i %-40(local/agenda-prefix) ")
        (tags . " %i %-40(local/agenda-prefix) ")
        (search . " %i %-40(local/agenda-prefix) ")))

;; Adapted from:
;; https://d12frosted.io/posts/2020-06-24-task-management-with-roam-vol2.html
(defun local/agenda-prefix ()
  "Get category of item at point for agenda.
Refer to `org-agenda-prefix-format' for more information."
  (let* ((file-name (when buffer-file-name
                      (file-name-sans-extension
                       (file-name-nondirectory buffer-file-name))))
         (title (vulpea-buffer-title-get))
         (category (org-get-category))
         (category-pretty (or (if (and
                                   title
                                   (string-equal category file-name))
                                  title
                                ;; there is no title, or the category is not the default one
                                category)
                              ""))
         (breadcrumbs (org-format-outline-path (take 2 (org-get-outline-path)) nil nil " > ")))
    (if (string-empty-p breadcrumbs) category-pretty
      (format "%s: %s" category-pretty breadcrumbs))))
