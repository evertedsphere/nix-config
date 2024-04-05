;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; (package! emacsql-sqlite :built-in 'prefer)
;; (package! org-roam :built-in 'prefer)
(package! org-roam-ui)
(package! citar-org-roam)
(package! org-protocol-capture-html)
;; (package! consult-org-roam)
(package! vulpea)
(package! ox-tufte)
(package! flycheck-projectile)
(package! org-web-tools)

;; org-slack-export-to-clipboard-as-slack
;; on the slack side, you can quickly apply the formatting by pressing Ctrl Shift F (Linux/Windows) or ⌘ Shift F (Mac).
(package! ox-slack)
;; (package! org-gcal)
(package! org-super-agenda)
(package! org-fc
  :recipe (
           :type git :repo "https://git.sr.ht/~l3kn/org-fc"
           :files (:defaults "awk" "demo.org"))
  )
(package! doct)
(package! named-timer)
(package! tokei)

(package! org-ql)
;; (package! embark :pin "e0ee1c78620c7cdc81bd786fb44bf0e2ee918e31")

(package! vterm :built-in t)
(package! fcitx)
(package! org-chef)
;; hide properties
;; (package! org-tidy)
(package! rg)

(package! keycast)
(package! gif-screencast)
