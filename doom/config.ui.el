;;; config.ui.el -*- lexical-binding: t; -*-

(setq doom-theme 'doom-tomorrow-night)
(setq doom-font (font-spec :family "Sarasa Mono J Nerd Font" :size 24 :weight 'semi-light)
      doom-big-font (font-spec :family "Sarasa Mono J Nerd Font" :size 30 :weight 'semi-light))
(setq display-line-numbers-type 'relative)
;; Only has an effect on emacs-pgtk, but let's leave it in.
(add-to-list 'default-frame-alist '(alpha-background . 90))

(setq evil-want-minibuffer nil)

(use-package! keycast
  :config
  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line (fix for use with doom-mode-line)."
    :global t
    (if keycast-mode
        (add-hook 'pre-command-hook 'keycast--update t)
      (remove-hook 'pre-command-hook 'keycast--update)))
  (add-to-list 'global-mode-string '("" keycast-mode-line))
  (keycast-mode))
