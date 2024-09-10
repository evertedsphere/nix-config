;;; config.nav.el -*- lexical-binding: t; -*-

(setq avy-all-windows t)
(map! :nv "C-s" 'avy-goto-char-timer)

(defun vertico-quick-embark (&optional arg)
  "Embark on candidate using quick keys."
  (interactive)
  (when (vertico-quick-jump)
    (embark-act arg)))
(map! :map vertico-map
      "C-i" #'vertico-quick-insert
      "M-i" #'vertico-quick-jump
      "C-'" #'vertico-quick-embark
      ;; use C-q ' for a literal quote
      "'" #'vertico-quick-exit)

(map!
 :map rust-mode-map
 :localleader
 :prefix ("m" . "custom")
 :desc "move up" :n "k" #'lsp-rust-analyzer-move-item-up
 :desc "move down" :n "j" #'lsp-rust-analyzer-move-item-down
 :desc "open docs" :n "d" #'lsp-rust-analyzer-open-external-docs)
