;;; config/lsp.el -*- lexical-binding: t; -*-

(after! lsp-rust
  (setq rustic-lsp-server 'rust-analyzer)
  (setq lsp-rust-server 'rust-analyzer))

(after! rustic
  (setq lsp-rust-server 'rust-analyzer)
  (setq rustic-lsp-server 'rust-analyzer))

(use-package! lsp
  :config
  (setq lsp-enable-symbol-highlighting nil
        ;; highlight colour codes in stuff like CSS
        lsp-enable-text-document-color t
        lsp-semantic-tokens-enable nil))

(use-package! lsp-ui
  :config
  (setq lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-position 'bottom
        lsp-ui-doc-side 'right
        lsp-ui-doc-delay 0.1
        lsp-ui-doc-use-webkit t
        lsp-ui-doc-max-height 40
        lsp-ui-doc-max-width 150
        ;; lsp-ui-imenu
        lsp-ui-imenu-auto-refresh t
        lsp-ui-imenu-window-fix-width t
        lsp-ui-peek-enable t))

(use-package! lsp-nix
  :after (lsp-mode)
  :custom
  (lsp-nix-nil-formatter ["alejandra"]))

(use-package! nix-mode
  :hook (nix-mode . lsp-deferred))

(set-formatter! 'rustfmt-2021 '("rustfmt" "--quiet" "--edition" "2021" "--emit" "stdout"))
(setq-hook! 'rust-mode-hook +format-with 'rustfmt-2021)

(set-formatter! 'pg-formatter '("pg_format" "-f" "1" "-U" "1" "-u" "1" "-s" "2"))
(setq-hook! 'sql-mode-hook +format-with 'pg-formatter)

(defun local/python-shell-send ()
  (interactive)
  (with-current-buffer (python-shell-get-buffer) (comint-clear-buffer))
  (python-shell-send-buffer))

(map! :localleader :map python-mode-map :desc "Clear and send buffer" :n "r" #'local/python-shell-send)
