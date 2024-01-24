;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(use-package! f)

(defun load-doom-dir-file (rel-path)
  (load-file (f-join doom-user-dir rel-path)))

(setq local-config-modules
      '("config.ui.el"
        "config.lsp.el"
        "config.org.el"
        "config.agenda.el"
        "config.input.el"
        "config.nav.el"
        "config.magit.el"))

(dolist (rel-path local-config-modules)
  (load-doom-dir-file rel-path))
