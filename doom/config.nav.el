;;; config.nav.el -*- lexical-binding: t; -*-

(map! :nv "C-s" 'avy-goto-char-2)

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
