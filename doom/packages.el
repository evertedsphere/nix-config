;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; (package! emacsql-sqlite :built-in 'prefer)
;; (package! org-roam :built-in 'prefer)
(package! org-roam-ui :pin "5ac74960231db0bf7783c2ba7a19a60f582e91ab")
(package! citar-org-roam :pin "82d47b5df1926627f56a09055c69b49b31cbbb9f")
(package! org-protocol-capture-html
  :recipe (:host github :repo "alphapapa/org-protocol-capture-html")
  :pin "a912aaefae8abdada2b2479aec0ad53fcf0b57bf")

;; (package! consult-org-roam)
(package! vulpea :pin "e1ea8480daf3e480effdd7ba3799126295a4a59a")
(package! ox-tufte :pin "ebdde02e4d33c3321543d67db8f1aef80adc03bd")
(package! flycheck-projectile :pin "ce6e9e8793a55dace13d5fa13badab2dca3b5ddb")
(package! org-web-tools :pin "7a6498f442fc7f29504745649948635c7165d847")

;; org-slack-export-to-clipboard-as-slack
;; on the slack side, you can quickly apply the formatting by pressing Ctrl Shift F (Linux/Windows) or ⌘ Shift F (Mac).
;; (package! ox-slack)
;; (package! org-gcal)
(package! org-super-agenda :pin "51c9da5ce7b791150758984bab469d2222516844")
;; (package! org-fc :recipe ( :type git :repo "https://git.sr.ht/~l3kn/org-fc" :files (:defaults "awk" "demo.org")))
(package! doct :pin "5cab660dab653ad88c07b0493360252f6ed1d898")
(package! named-timer :pin "d8baeada19b56176c66aed5fa220751e3de11cb8")
(package! tokei :pin "86fbca422f580a95eb30247e46891184f3ac5c18")

(package! org-ql :pin "c9370982bfd4df04b590762bd795a7da3012c4dd")
;; (package! embark :pin "e0ee1c78620c7cdc81bd786fb44bf0e2ee918e31")

(package! vterm :pin "df057b1af2bb89a1deb288086f13be296af42090")
(package! fcitx :pin "b399482ed8db5893db2701df01db4c38cccda495")
(package! org-chef :pin "1710b54441ed744dcdfb125d08fb88cfaf452f10")
;; hide properties
;; (package! org-tidy :pin "")
(package! rg :pin "5420dc6ca05c6ab0954113772d784c4c968ca219")

(package! keycast :pin "04fa2c65f0ae901ed3015f691ea70f7658ea24b8")
(package! gif-screencast :pin "6798656d3d3107d16e30cc26bc3928b00e50c1ca")

(package! org-transclusion)
(package! anki-editor
  :recipe (:host github :repo "anki-editor/anki-editor")
  :pin "cbdca2eb2e2064781b177a0f8d6bc94b95de0e8e")

(package! anki-editor-ui
  :recipe (:host github :repo "anki-editor/anki-editor")
  :pin "cbdca2eb2e2064781b177a0f8d6bc94b95de0e8e")

;; (package! org-drill :pin "")
(package! org-download :pin "19e166f0a8c539b4144cfbc614309d47a9b2a9b7")

(package! symex :pin "36f57dc94d225fa326191d46bc7ec74111fbf41d")
(package! pcre2el :pin "b4d846d80dddb313042131cf2b8fbf647567e000")

(package! org-music
  :pin "1669b8488a431ce70adbbbd1b6ed1f467aa8c1ef"
  :recipe (:host github :repo "tecosaur/org-music"))

(package! emprise
  :pin "669a46f6027acdf1673c044bda0e2216e794b812"
  :recipe (:host nil :repo "https://git.sr.ht/~plattfot/emprise"))

(package! marginalia-emprise
  :pin "d371d1e1691dd653e59777cbf80dbb4877b39172"
  :recipe (:host nil :repo "https://git.sr.ht/~plattfot/marginalia-emprise"))
