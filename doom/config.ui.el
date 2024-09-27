;; config.ui.el -*- lexical-binding: t; -*-

(pixel-scroll-precision-mode)

;; (set-frame-parameter (selected-frame) 'alpha-background 90)
;; (add-to-list 'default-frame-alist '(alpha-background . 90))

(setq doom-theme 'doom-gruvbox)

(let
    ((font-size
      (cond
       ((string= (system-name) "malina") 26)
       ((string= (system-name) "work") 30)
       (t 30))))
  (setq doom-font (font-spec :family "Iosevka Comfy" :size font-size :weight 'normal)))

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
  (add-to-list 'global-mode-string '("" keycast-mode-line)))

(use-package! gif-screencast
  :config
  ;; TODO $HOME:$XDG_VIDEOS_DIR
  (setq gif-screencast-output-directory "~/media")
  (setq gif-screencast-countdown 0))
(map! "<f9>" 'gif-screencast-start-or-stop)


(setq which-key-allow-multiple-replacements t)
(after! which-key
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "E:\\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "E:\\1"))))

(after! marginalia
  (setq marginalia-censor-variables nil)

  (defadvice! +marginalia--annotate-local-file-colorful (cand)
    "Just a more colourful version of `marginalia--annotate-local-file'."
    :override #'marginalia--annotate-local-file
    (when-let (attrs (file-attributes (substitute-in-file-name
                                       (marginalia--full-candidate cand))
                                      'integer))
      (marginalia--fields
       ((marginalia--file-owner attrs)
        :width 12 :face 'marginalia-file-owner)
       ((marginalia--file-modes attrs))
       ((+marginalia-file-size-colorful (file-attribute-size attrs))
        :width 7)
       ((+marginalia--time-colorful (file-attribute-modification-time attrs))
        :width 12))))

  (defun +marginalia--time-colorful (time)
    (let* ((seconds (float-time (time-subtract (current-time) time)))
           (color (doom-blend
                   (face-attribute 'marginalia-date :foreground nil t)
                   (face-attribute 'marginalia-documentation :foreground nil t)
                   (/ 1.0 (log (+ 3 (/ (+ 1 seconds) 345600.0)))))))
      ;; 1 - log(3 + 1/(days + 1)) % grey
      (propertize (marginalia--time time) 'face (list :foreground color))))

  (defun +marginalia-file-size-colorful (size)
    (let* ((size-index (/ (log (+ 1 size)) 7.0))
           (color (if (< size-index 10000000) ; 10m
                      (doom-blend 'orange 'green size-index)
                    (doom-blend 'red 'orange (- size-index 1)))))
      (propertize (file-size-human-readable size) 'face (list :foreground color)))))

(after! doom-modeline
  (setq auto-revert-check-vc-info t
        doom-modeline-major-mode-icon nil
        doom-modeline-buffer-file-name-style 'relative-to-project
        doom-modeline-github nil
        doom-modeline-vcs-max-length 60)
  (remove-hook 'doom-modeline-mode-hook #'size-indication-mode)
  (doom-modeline-def-segment lispyville '(" " (if (lispyville--lispy-keybindings-active-p) "active" "inactive") "!"))
  (doom-modeline-def-modeline 'main
    '(matches modals workspace-name window-number persp-name selection-info buffer-info remote-host debug vcs matches)
    '(github mu4e grip gnus misc-info repl lsp " ")))

(use-package! symex
  :custom
  (symex-modal-backend 'evil)
  (symex-quote-prefix-list (list "'" "`" "#'" "#`"))
  (symex-unquote-prefix-list (list "," ",@" "#,@"))
  :config
  (setq symex--user-evil-keyspec
        '(("j" . symex-go-up)
          ("k" . symex-go-down)
          ("C-j" . symex-climb-branch)
          ("C-k" . symex-descend-branch)
          ("M-j" . symex-goto-highest)
          ("M-k" . symex-goto-lowest)))
  (symex-initialize)
  (setq symex-common-lisp-backend 'slime)
  (evil-define-key 'normal symex-mode-map
    (kbd "<escape>") 'symex-mode-interface)
  (evil-define-key 'insert symex-mode-map
    (kbd "<escape>") 'symex-mode-interface))

;; https://github.com/joaotavora/sly/issues/535
;; https://github.com/larrasket/doomemacs/commit/01f560ab86d79b5d39ca350789ced2563b570f8d
(use-package! sly
  :config
  (setq sly-complete-symbol-function 'sly-flex-completions))

(map! :nvi "M-h" #'evil-window-left)
(map! :nvi "M-l" #'evil-window-right)
(map! :nvi "M-j" #'evil-window-down)
(map! :nvi "M-k" #'evil-window-up)

(use-package! emprise)

(use-package! flymake
  :init
  (global-set-key [remap previous-error] #'flymake-goto-prev-error)
  (global-set-key [remap next-error] #'flymake-goto-next-error))

(after! consult
  (setq read-file-name-function #'consult-find-file-with-preview)
  (defun consult-find-file-with-preview (prompt &optional dir default mustmatch initial pred)
    (interactive)
    (let ((default-directory (or dir default-directory))
          (minibuffer-completing-file-name t))
      (consult--read #'read-file-name-internal
                     :state (consult--file-preview)
                     :prompt prompt
                     :initial initial
                     :require-match mustmatch
                     :predicate pred))))

(after! orderless
  (setq orderless-matching-styles '(orderless-literal orderless-regexp)))
