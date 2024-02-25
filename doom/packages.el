;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; (package! emacsql-sqlite :built-in 'prefer)
;; (package! org-roam :built-in 'prefer)
(package! org-roam-ui)
(package! citar-org-roam)
(package! org-protocol-capture-html)
(package! consult-org-roam)
(package! vulpea)
(package! ox-tufte)
(package! flycheck-projectile)

(unpin! jupyter)
(package! org-ql :pin "d09608aa35090df8a02cc372e9c862566b0169f4")

(package! vterm :built-in t)
(package! fcitx)
(package! org-chef)
;; hide properties
(package! org-tidy)
