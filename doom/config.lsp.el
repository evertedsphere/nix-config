;;; config/lsp.el -*- lexical-binding: t; -*-

(after! lsp-rust
  (setq rustic-lsp-server 'rust-analyzer)
  (setq lsp-rust-server 'rust-analyzer))

(after! rustic
  (setq lsp-rust-server 'rust-analyzer)
  (setq rustic-lsp-server 'rust-analyzer))

(use-package! lsp
  :preface
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))
  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)                             ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection))  ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
              (setcar orig-result command-from-exec-path))
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))
  :init
  (setq lsp-use-plists t)
  :config
  (setq lsp-enable-symbol-highlighting nil
        ;; highlight colour codes in stuff like CSS
        lsp-enable-text-document-color t
        ;; no
        lsp-enable-suggest-server-download nil
        lsp-lens-enable nil
        lsp-semantic-tokens-enable nil)
  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))

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


(use-package! slime
  :defer t ; don't load the package immediately
  :init ; runs this immediately
  (setq inferior-lisp-program "sbcl")
  :config ; runs this when slime loads
  (set-repl-handler! 'lisp-mode #'sly-mrepl)
  (set-eval-handler! 'lisp-mode #'sly-eval-region)
  (set-lookup-handlers! 'lisp-mode
    :definition #'sly-edit-definition
    :documentation #'sly-describe-symbol)

  (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)

  (defun my--slime-completion-at-point ()
    (let ((slime-current-thread :repl-thread)
          (package (slime-current-package)))
      (when-let ((symbol (thing-at-point 'symbol)))
        (pcase-let ((`(,beg . ,end)
                     (bounds-of-thing-at-point 'symbol)))
          (list beg end
                (car (slime-eval
                      ;; Or swank:simple-completions
                      `(swank:fuzzy-completions
                        ,(substring-no-properties symbol) ',package))))))))
  (advice-add #'slime--completion-at-point
              :override #'my--slime-completion-at-point))


