;;; config.ui.el -*- lexical-binding: t; -*-

(setq doom-theme 'modus-vivendi)
(setq doom-font (font-spec :family "Sarasa Mono J Nerd Font" :size 24 :weight 'semi-light)
      doom-big-font (font-spec :family "Sarasa Mono J Nerd Font" :size 30 :weight 'semi-light))
(setq display-line-numbers-type 'relative)
;; Only has an effect on emacs-pgtk, but let's leave it in.
(add-to-list 'default-frame-alist '(alpha-background . 90))
