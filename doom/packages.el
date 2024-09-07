;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! org-roam-ui :pin "5ac74960231db0bf7783c2ba7a19a60f582e91ab")
(package! citar-org-roam :pin "82d47b5df1926627f56a09055c69b49b31cbbb9f")
(package! org-protocol-capture-html
  :recipe (:host github :repo "alphapapa/org-protocol-capture-html")
  :pin "a912aaefae8abdada2b2479aec0ad53fcf0b57bf")

;; (package! consult-org-roam)
(package! vulpea :pin "f5c7a68b5308336927d24a166681a2a1903289c3")
(package! ox-tufte :pin "ebdde02e4d33c3321543d67db8f1aef80adc03bd")
(package! flycheck-projectile :pin "ce6e9e8793a55dace13d5fa13badab2dca3b5ddb")
(package! org-web-tools :pin "7a6498f442fc7f29504745649948635c7165d847")

;; org-slack-export-to-clipboard-as-slack
;; on the slack side, you can quickly apply the formatting by pressing Ctrl Shift F (Linux/Windows) or ⌘ Shift F (Mac).
;; (package! ox-slack)
;; (package! org-gcal)
(package! org-super-agenda :pin "05a710065af5ee4b3982f9619f864f7af12ca1d3")
(package! doct :pin "5cab660dab653ad88c07b0493360252f6ed1d898")
(package! named-timer :pin "d8baeada19b56176c66aed5fa220751e3de11cb8")
(package! tokei :pin "86fbca422f580a95eb30247e46891184f3ac5c18")

(package! org-ql :pin "fcb4e3ee62d2c6f9d2a9a8a510a50dd1a6eceb5e")

(package! vterm :pin "988279316fc89e6d78947b48513f248597ba969a")
(package! fcitx :pin "b399482ed8db5893db2701df01db4c38cccda495")
;; hide properties
;; (package! org-tidy :pin "")
(package! rg :pin "5420dc6ca05c6ab0954113772d784c4c968ca219")

(package! keycast :pin "04fa2c65f0ae901ed3015f691ea70f7658ea24b8")
(package! gif-screencast :pin "6798656d3d3107d16e30cc26bc3928b00e50c1ca")

(package! org-transclusion
  :recipe (:host github :repo "nobiot/org-transclusion")
  :pin "e6e638710e90198070c9b07ebdaa345a79f74706")

(package! anki-editor
  :recipe (:host github :repo "anki-editor/anki-editor")
  :pin "ba7c7bf3269f7630ef8c06f342ab04bdd8efea53")
(package! anki-editor-ui
  :recipe (:host github :repo "anki-editor/anki-editor")
  :pin "ba7c7bf3269f7630ef8c06f342ab04bdd8efea53")

;; (package! org-drill :pin "")
(package! org-download :pin "19e166f0a8c539b4144cfbc614309d47a9b2a9b7")

(package! symex :pin "fe45b0bb13a6a906cc6ac7d836a4021ee4a82421")
(package! pcre2el :pin "b4d846d80dddb313042131cf2b8fbf647567e000")

(package! emprise
  :pin "669a46f6027acdf1673c044bda0e2216e794b812"
  :recipe (:host nil :repo "https://git.sr.ht/~plattfot/emprise"))

(package! sly :disable t)
(package! sly-macrostep :disable t)
(package! sly-repl-ansi-color :disable t)

(package! slime)

(package! el-patch)
