;;; config.input.el -*- lexical-binding: t; -*-

;; Set up integration with fcitx5-based input methods.
;; In particular, this will switch to the main input method (for me, a standard
;; English (US) layout) when entering normal mode or in the minibuffer.
(use-package! fcitx
  :if (not (string= (system-name) "work"))
  :config
  (setq fcitx-remote-command "fcitx5-remote")
  (fcitx-aggressive-setup)
  (setq fcitx-use-dbus nil))
