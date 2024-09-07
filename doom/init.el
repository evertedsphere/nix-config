;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load
;; in. Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find a link to Doom's Module Index where all
;;      of our modules are listed, including what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c c k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c c d') on a module to browse its
;;      directory (for easy access to its source code).

(doom! :input

       :completion
       (corfu +orderless +icons)
       (vertico +orderless +icons)

       :ui
       doom
       doom-dashboard
       ;;doom-quit
       ;; (emoji +unicode)
       hl-todo
       ;; indent-guides
       ligatures
       ;;minimap
       modeline
       nav-flash
       ;;neotree
       ophints
       (popup +defaults)
       ;;tabs
       ;;treemacs
       ;;unicode
       (vc-gutter +pretty)
       ;; vi-tilde-fringe
       ;;window-select
       workspaces
       zen

       :editor
       (evil +everywhere)
       file-templates
       fold
       (format +onsave +lsp)
       ;; lispy
       ;; multiple-cursors
       ;;objed
       ;;parinfer
       ;;rotate-text
       snippets
       word-wrap

       :emacs
       (dired +icons +dirvish)
       electric
       ibuffer
       (undo +tree)
       vc

       :term
       eshell
       vterm

       :checkers
       (syntax +icons +childframe +flymake)
       ;;(spell +flyspell)
       ;;grammar

       :tools
       ;;ansible
       biblio
       (debugger +lsp)
       direnv
       ;;docker
       ;;editorconfig
       ;; ein
       (eval +overlay)
       lookup
       lsp
       (magit +forge)
       ;;make
       ;;(pass +auth)
       ;;pdf
       ;;prodigy
       ;;rgb
       ;;taskrunner
       ;;terraform
       ;;tmux
       ;; tree-sitter
       ;;upload

       :os
       (:if IS-MAC macos)
       ;;tty

       :lang
       ;;agda
       ;;beancount
       cc
       ;;clojure
       ;; common-lisp
       ;;coq
       ;;crystal
       ;;csharp
       data
       ;;(dart +flutter)
       ;;dhall
       ;;(elixir +lsp)
       ;;elm
       emacs-lisp
       ;;erlang
       ;;ess
       ;;factor
       ;;faust
       ;;fortran
       ;;fsharp
       ;;fstar
       ;;(gdscript +lsp)
       ;;(go +lsp)
       ;;(graphql +lsp)
       (haskell +lsp)
       ;;hy
       ;;idris
       json
       ;;(java +lsp)
       ;; Can't enable tree-sitter because it doesn't work well yet with nix-doom-emacs-unstraightened
       ;; https://github.com/marienz/nix-doom-emacs-unstraightened/commit/c8011f8f101b3e4ec1ebb73794f9ceb82c81bbc5
       (javascript +lsp)
       ;;julia
       ;;kotlin
       ;;latex
       ;;lean
       ;;ledger
       ;;lua
       markdown
       ;;nim
       (nix +lsp)
       ;;(ocaml +lsp +tree-sitter)
       (org +pretty
            +roam2
            +dragndrop
            ;; +hugo
            +babel
            ;; +gnuplot
            ;; +pandoc
            ;; +noter
            +pomodoro
            )
       ;;php
       ;;plantuml
       ;;purescript
       (python +lsp +pyright +poetry)
       ;;qt
       ;; racket
       ;;raku
       ;;rest
       ;;rst
       ;;(ruby +rails)
       (rust +lsp)
       ;; (scala +lsp +tree-sitter)
       ;; (scheme +guile)
       sh
       ;;sml
       ;;solidity
       ;;swift
       ;;terra
       web
       yaml
       ;;zig

       :email
       ;;(mu4e +org +gmail)
       ;;notmuch
       ;;(wanderlust +gmail)

       :app
       ;;calendar
       ;;;emms
       ;;everywhere
       ;;irc
       ;;(rss +org)

       :config
       ;;literate
       (default +bindings +smartparens))
