;;; config.magit.el -*- lexical-binding: t; -*-

;; Refresh the magit status buffer after any edits performed within Emacs.
;; Note that this does not set file watchers on the files in the repo or
;; anything similar, so changes e.g. made by a formatter will not be picked up.
(after! magit
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t))
