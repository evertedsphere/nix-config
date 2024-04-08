;;; config.anki.el -*- lexical-binding: t; -*-

(use-package! anki-editor)
(use-package! anki-editor-ui
  :after anki-editor)

(use-package! org-drill
  :after org)

(add-load-path! "lisp")

(use-package! pcre2el)

(use-package! org-roam-review
  :after (org-drill pcre2el)

  :commands (org-roam-review
             org-roam-review-list-by-maturity
             org-roam-review-list-recently-added
             org-roam-search
             org-roam-links

             org-roam-rewrite-rename
             org-roam-rewrite-remove
             org-roam-rewrite-inline
             org-roam-rewrite-extract
             )

  :hook (org-mode . org-roam-dblocks-autoupdate-mode)
  :config
  (require 'org-roam-dblocks)
  (require 'org-roam-search)
  (require 'org-roam-links)

  (require 'org-roam-slipbox)
  (org-roam-slipbox-buffer-identification-mode +1)
  (org-roam-slipbox-tag-mode +1)
  (add-hook 'org-mode-hook #'org-roam-slipbox-buffer-identification-mode)
  (add-hook 'org-mode-hook #'org-roam-slipbox-tag-mode)

  (require 'org-roam-rewrite)

  ;; -----------------------------------
  (require 'org-roam-review)
  ;; org-roam-review
  ;; ;; Optional - tag all newly-created notes as seedlings.
  ;; :hook (org-roam-capture-new-node . org-roam-review-set-seedling)

  :bind (:map org-mode-map
              ("C-c r r" . org-roam-review-accept)
              ("C-c r u" . org-roam-review-bury)
              ("C-c r x" . org-roam-review-set-excluded)
              ("C-c r b" . org-roam-review-set-budding)
              ("C-c r s" . org-roam-review-set-seedling)
              ("C-c r e" . org-roam-review-set-evergreen))

  :general
  (:states '(normal) :keymaps 'org-roam-review-mode-map
           "TAB" 'magit-section-cycle
           "g r" 'org-roam-review-refresh)

  )
