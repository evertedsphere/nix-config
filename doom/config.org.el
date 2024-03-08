;;; config/org.el -*- lexical-binding: t; -*-

(setq org-directory "~/o/")
(defvar local/org-roam-subdir "kb"
  "Subdirectory of org-directory to use for org-roam.")
(setq org-roam-directory (f-join org-directory local/org-roam-subdir))
(defvar local/org-sync-subdir "sync"
  "Subdirectory of org-roam-directory to sync.")
(setq org-sync-directory (f-join org-roam-directory local/org-sync-subdir))
(setq org-agenda-files (list org-directory org-roam-directory org-sync-directory (f-join org-roam-directory "daily/journal.org")))
(setq org-default-notes-file (f-join org-roam-directory "inbox.org"))

(use-package! org
  :init
  (defun display-ansi-colors ()
    "Fixes kernel output in emacs-jupyter"
    (ansi-color-apply-on-region (point-min) (point-max)))
  :hook
  (org-mode . (lambda () (add-hook 'org-babel-after-execute-hook #'display-ansi-colors))))

;; (use-package! org-tidy
;;   :after org
;;   :hook
;;   (org-mode . org-tidy-mode))

(use-package! org-habit
  :after org
  :config
  (setq org-habit-following-days 7
        org-habit-preceding-days 35
        org-habit-show-habits t))

(setq org-capture-templates
      `(("i" "inbox" entry (file ,org-default-notes-file)
         "* TODO %?")
        ;; https://github.com/alphapapa/org-protocol-capture-html
        ("w" "Web site" entry (file ,org-default-notes-file)
         "* %a :website:\n%U\n%:initial" :immediate-finish t)
        ("s" "Web site (whole page)" entry (file ,org-default-notes-file)
         "%(org-web-tools--url-as-readable-org \"%L\")"
         :immediate-finish t)
        ("c" "org-protocol-capture" entry (file ,org-default-notes-file)
         "* TODO [[%:link][%:description]]\n\n %i" :immediate-finish t)
        ("k" "Cookbook" entry (file "~/o/cookbook.org")
         "%(org-chef-get-recipe-from-url)"
         :empty-lines 1)
        ("m" "Manual Cookbook" entry (file "~/org/cookbook.org")
         "* %^{Recipe title: }\n  :PROPERTIES:\n  :source-url:\n  :servings:\n  :prep-time:\n  :cook-time:\n  :ready-in:\n  :END:\n** Ingredients\n   %?\n** Directions\n\n")
        ))

(after! org


  (setq org-protocol-default-template-key "c")
  (setq org-agenda-skip-scheduled-if-done t
        org-agenda-show-future-repeats nil
        org-clock-continuously t
        org-agenda-skip-deadline-if-done t
        org-todo-keywords '((sequence "TODO(t)" "WAIT(w)" "|" "INACTIVE(i)" "DONE(d)" "CANCELED(x)"))
        org-todo-keywords-for-agenda '((sequence "TODO(t)" "WAIT(w)" "|" "INACTIVE(i)" "DONE(d)" "CANCELED(x)"))))

(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil))

(after! org-roam
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %?"
           :clock-in t :clock-resume t
           :target (file+datetree "journal.org" day)))))

(defun local/tag-new-node-as-draft ()
  (org-roam-tag-add '("draft")))

(add-hook 'org-roam-capture-new-node-hook #'local/tag-new-node-as-draft)

(advice-remove #'org-babel-do-load-languages #'ignore)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell . t)
   (python . t)))
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)



(require 'org-chef-utils)
(require 'dom)

(advice-add 'org-chef-xiachufang-extract-ingredients :override
            #'(lambda (dom)
                "Get the ingredients for a recipe from a xiachufang DOM."
                (split-string
                 (car (mapcar #'(lambda (n) (org-chef-xiachufang-sanitize (dom-texts n "\n")))
                              (dom-elements dom 'class "^ings$")))
                 "\n")))

(advice-add 'org-chef-xiachufang-extract-directions :override
            #'(lambda (dom)
                "Get the directions for a recipe from a xiachufang DOM."
                (split-string
                 (car (mapcar #'(lambda (n) (org-chef-xiachufang-sanitize (dom-texts n "\n")))
                              (dom-elements dom 'class "^steps$")))
                 "\n")))

;; hack for the annoying org-element warnings
(setq warning-minimum-level :error)
