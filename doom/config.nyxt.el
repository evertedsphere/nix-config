;;; config.nyxt.el -*- lexical-binding: t; -*-

(load "~/common-lisp/nyxt/build-scripts/nyxt-guix.el" :noerror)

(setq sly-lisp-implementations
      '((nyxt-sbcl-2
         (lambda () (nyxt-make-guix-cl-for-nyxt
                     "~/common-lisp/nyxt"
                     :force t
                     :cl-implementation "sbcl"
                     :cl-system "nyxt/gi-gtk"
                     :no-grafts t
                     :ad-hoc '("emacs" "xdg-utils" "git"))))))
