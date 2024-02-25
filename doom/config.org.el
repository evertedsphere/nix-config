;;; config/org.el -*- lexical-binding: t; -*-

(setq org-directory "~/o/")
(defvar local/org-roam-subdir "kb"
  "Subdirectory of org-directory to use for org-roam.")
(setq org-roam-directory (f-join org-directory local/org-roam-subdir))
(setq org-agenda-files (list org-directory org-roam-directory (f-join org-directory "daily/")))
(setq org-default-notes-file (f-join org-directory "inbox.org"))

(use-package! org
  :init
  (defun display-ansi-colors ()
    "Fixes kernel output in emacs-jupyter"
    (ansi-color-apply-on-region (point-min) (point-max)))
  :hook
  (org-mode . (lambda () (add-hook 'org-babel-after-execute-hook #'display-ansi-colors))))

(use-package! org-habit
  :after org
  :config
  (setq org-habit-following-days 7
        org-habit-preceding-days 35
        org-habit-show-habits t))

(after! org
  (setq org-capture-templates
        `(("i" "inbox" entry (file ,org-default-notes-file)
           "* TODO %?")
          ("w" "Web site"
           entry (file+olp ,org-default-notes-file "Web")
           "* %c :website:\n%U %?%:initial")
          ("c" "org-protocol-capture" entry (file ,org-default-notes-file)
           "* TODO [[%:link][%:description]]\n\n %i" :immediate-finish t)
          ("k" "Cookbook" entry (file "~/o/cookbook.org")
           "%(org-chef-get-recipe-from-url)"
           :empty-lines 1)
          ("m" "Manual Cookbook" entry (file "~/org/cookbook.org")
           "* %^{Recipe title: }\n  :PROPERTIES:\n  :source-url:\n  :servings:\n  :prep-time:\n  :cook-time:\n  :ready-in:\n  :END:\n** Ingredients\n   %?\n** Directions\n\n")
          ))
  (setq org-protocol-default-template-key "c"))

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
        '(("d" "default" entry "* <%<%F %a %H:%M>> %?" :target
           (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n")))))

(defun local/tag-new-node-as-draft ()
  (org-roam-tag-add '("draft")))

(add-hook 'org-roam-capture-new-node-hook #'local/tag-new-node-as-draft)

(use-package! consult-org-roam
  :ensure t
  :after org-roam
  :init
  (require 'consult-org-roam)
  ;; Activate the minor mode
  (consult-org-roam-mode 1)
  :custom
  ;; Use `ripgrep' for searching with `consult-org-roam-search'
  (consult-org-roam-grep-func #'consult-ripgrep)
  ;; Configure a custom narrow key for `consult-buffer'
  (consult-org-roam-buffer-narrow-key ?r)
  ;; Display org-roam buffers right after non-org-roam buffers
  ;; in consult-buffer (and not down at the bottom)
  (consult-org-roam-buffer-after-buffers t)
  :config
  ;; Eventually suppress previewing for certain functions
  (consult-customize
   consult-org-roam-forward-links
   :preview-key (kbd "M-."))
  :bind
  ;; Define some convenient keybindings as an addition
  ("C-c n e" . consult-org-roam-file-find)
  ("C-c n b" . consult-org-roam-backlinks)
  ("C-c n l" . consult-org-roam-forward-links)
  ("C-c n r" . consult-org-roam-search))

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
