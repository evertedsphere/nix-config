;;; config/lsp.el -*- lexical-binding: t; -*-

(after! lsp-rust
  (setq rustic-lsp-server 'rust-analyzer)
  (setq lsp-rust-server 'rust-analyzer))
(after! rustic
  (setq lsp-rust-server 'rust-analyzer)
  (setq rustic-lsp-server 'rust-analyzer))

(use-package! lsp
  :config
  (setq lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-use-webkit t
        lsp-enable-symbol-highlighting nil
        lsp-ui-doc-max-height 20
        lsp-ui-doc-max-width 110))

;; nil setup
;;
;; (use-package lsp-nix
;;   :ensure lsp-mode
;;   :after (lsp-mode)
;;   :demand t
;;   :custom
;;   (lsp-nix-nil-formatter ["nixpkgs-fmt"]))

;; (use-package nix-mode
;;   :hook (nix-mode . lsp-deferred)
;;   :ensure t)

(with-eval-after-load 'lsp-mode
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "nixd")
                    :major-modes '(nix-mode)
                    :priority 0
                    :server-id 'nixd)))

(setq +format-with-lsp t)

(set-formatter! 'rustfmt-2021 '("rustfmt" "--quiet" "--edition" "2021" "--emit" "stdout"))
(setq-hook! 'rust-mode-hook +format-with 'rustfmt-2021)

(set-formatter! 'pg-formatter '("pg_format" "-f" "1" "-U" "1" "-u" "1" "-s" "2"))
(setq-hook! 'sql-mode-hook +format-with 'pg-formatter)

(setq-hook! 'haskell-mode-hook +format-with-lsp t)
(add-hook 'haskell-mode-hook (lambda () (add-hook 'before-save-hook #'lsp-format-buffer nil t)))

(defun local/python-shell-send ()
  (interactive)
  (with-current-buffer (python-shell-get-buffer) (comint-clear-buffer))
  (python-shell-send-buffer))

(map! :localleader :map python-mode-map :desc "Clear and send buffer" :n "r" #'local/python-shell-send)
