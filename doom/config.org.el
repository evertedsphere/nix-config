;;; config/org.el -*- lexical-binding: t; -*-

(use-package! org
  :init
  (defun display-ansi-colors ()
    "Fixes kernel output in emacs-jupyter"
    (ansi-color-apply-on-region (point-min) (point-max)))
  :hook
  (org-mode . (lambda () (add-hook 'org-babel-after-execute-hook #'display-ansi-colors))))

;; (use-package! org-tidy
;;   :after org
;;   :hook
;;   (org-mode . org-tidy-mode))

(use-package! org-habit
  :after org
  :config
  (setq org-habit-following-days 7
        org-habit-preceding-days 35
        org-habit-show-habits t
        org-habit-show-all-today t))

(setq org-directory "~/o/")

(defvar local/org-roam-subdir "kb"
  "Subdirectory of org-directory to use for org-roam.")
(defvar local/org-work-subdir "work"
  "Subdirectory of org-directory to use for work tasks.")
(defvar local/org-sync-subdir "sync"
  "Subdirectory of org-roam-directory to sync.")
(defvar local/org-lit-subdir "lit"
  "Subdirectory of org-roam-directory to use for literature notes.")

(setq local/org-roam-dir (f-join org-directory local/org-roam-subdir))
(setq local/org-work-dir (f-join local/org-roam-dir local/org-work-subdir))
(setq local/org-sync-dir (f-join local/org-roam-dir local/org-sync-subdir))
(setq local/org-lit-dir (f-join local/org-roam-dir local/org-lit-subdir))

(setq org-roam-directory
      (cond
       ((string= (system-name) "malina") local/org-roam-dir)
       ((string= (system-name) "work") local/org-work-dir)
       (t local/org-roam-dir)))

(setq org-agenda-files '())

(setq org-default-notes-file (f-join org-roam-directory "inbox.org"))
(setq +org-capture-notes-file org-default-notes-file)
(setq org-protocol-default-template-key "c")
(setq org-clock-continuously nil
      org-clock-persist t
      org-extend-today-until 0
      org-habit-preceding-days 5
      org-startup-folded 'show2levels
      org-clock-into-drawer "CLOCK_LOG"
      org-hide-emphasis-markers t
      org-log-done 'time
      org-log-into-drawer "STATE_CHANGES"
      org-image-actual-width '(768)
      org-todo-keywords '((sequence "TODO(t)" "WAIT(w)" "|" "INACTIVE(i!)" "DONE(d!)" "CANCELLED(x!)"))
      org-todo-keywords-for-agenda '((sequence "TODO(t)" "WAIT(w)" "|" "INACTIVE(i!)" "DONE(d!)" "CANCELLED(x!)"))
      org-agenda-hide-tags-regexp (rx-to-string '(or "draft" "project"))
      org-attach-id-dir ".attachments/")


(setq org-capture-templates
      `(("i" "inbox" entry (file ,org-default-notes-file)
         "* TODO %?")
        ;; https://github.com/alphapapa/org-protocol-capture-html
        ("w" "Web site" entry (file ,org-default-notes-file)
         "* %a :website:\n%U\n%:initial" :immediate-finish t)
        ("s" "Web site (whole page)" entry (file ,org-default-notes-file)
         "%(org-web-tools--url-as-readable-org \"%L\")"
         :immediate-finish t)
        ("c" "org-protocol-capture" entry (file ,org-default-notes-file)
         "* TODO [[%:link][%:description]]\n\n %i" :immediate-finish t)
        ("m" "Manual Cookbook" entry (file "~/o/cookbook.org")
         "* %^{Recipe title: }\n  :PROPERTIES:\n  :source-url:\n  :servings:\n  :prep-time:\n  :cook-time:\n  :ready-in:\n  :END:\n** Ingredients\n   %?\n** Directions\n\n")
        ))

(setq! org-super-agenda-final-group-separator ?┄)

(defun local/deadline-or-scheduled-on (when-scheduled)
  "Generates an org-super-agenda-mode selector for items with a deadline on or
scheduled for the given date."
  `(:and (:deadline ,when-scheduled :not (:habit t))
    :and (:scheduled ,when-scheduled :not (:habit t))))

(setq org-super-agenda-groups
      `((:name "Log"
         :log t)
        (:name "Important"
         :priority "A")
        (:name "Overdue" ,@(local/deadline-or-scheduled-on 'past))
        (:name "Today" ,@(local/deadline-or-scheduled-on 'today))
        (:name "Less important"
         :priority "B")
        (:name "L2"
         :tag "l2@read"
         :tag "l2@listen"
         :tag "l2@drill")
        (:name "Consistency checks" :tag "habit" :habit t)))

;; TODO: keep everything but hjkl
(setq org-super-agenda-header-map (make-sparse-keymap))

(after! org
  (org-clock-persistence-insinuate))

(defun local/switch-to-agenda ()
  (interactive)
  (let ((org-agenda-start-day "-1d")
        (org-agenda-span 3))
    (org-agenda nil "a")
    (org-super-agenda-mode)
    ;; uhh
    (org-agenda-redo)))

(use-package! org-agenda
  :init
  (map! "<f1>" #'local/switch-to-agenda)
  (setq org-agenda-block-separator nil
        org-agenda-skip-scheduled-if-done t
        org-agenda-show-future-repeats t
        org-agenda-skip-deadline-if-done t
        org-agenda-sticky nil
        org-agenda-start-with-log-mode t
        org-agenda-use-time-grid nil
        ;; the state change items are superfluous
        org-agenda-log-mode-items '(closed clock))
  ;; from jethrokuan
  )

(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil))

;; TODO add template entries here
;; each logs into a datetree in a file/heading
;; the habit metadata could be updated via some code that runs around capture…
;; the subheading's clock duration would be imputed to the parent, so that would be easily sorted
(use-package! org-roam
  :config
  (add-hook 'org-roam-mode-hook #'turn-on-visual-line-mode)
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?" :target
           (file+head "%<%Y%m%d%H%M%S>.org" "#+title: ${title}\n")
           :unnarrowed t)))
  (setq org-roam-dailies-directory "daily/")
  (setq org-roam-dailies-capture-templates
        (let ((base '(entry "* %?" :target
                      (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
          `(("q" "quick" ,@base)
            ("d" "leave open" ,@base :clock-in t :clock-resume t)
            ("l" "background" ,@base :clock-in t :clock-keep t)))))

;; from jethrokuan
(defun local/tag-new-node-as-draft ()
  (org-roam-tag-add '("draft")))

(add-hook 'org-roam-capture-new-node-hook #'local/tag-new-node-as-draft)

;; doom is smart, too smart
(advice-remove #'org-babel-do-load-languages #'ignore)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell . t)
   (python . t)))
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

(setq org-structure-template-alist
      '(("a" . "export ascii")
        ("c" . "center")
        ("C" . "comment")
        ("e" . "example")
        ("E" . "export")
        ("h" . "export html")
        ("l" . "export latex")
        ("q" . "quote")
        ("s" . "src")
        ("v" . "verse")
        ("pt" . "src python :results value :post matrix(*this*) :exports results")
        ("po" . "src python :results output :exports both")
        ("pv" . "src python :results value :exports both")
        ("ps" . "src python :results silent :exports code")))


;; hack for the annoying org-element warnings
(setq warning-minimum-level :error)

(message "%s" "Have you tried being normal?")
;; stolen from neeasade's :harass-myself timer

(use-package! named-timer)

(require 'notifications)
(require 'named-timer)
(require 'org-pomodoro)

(defvar clock-check-interval 10
  "How often to harass the user if they are not clocked in.")
(defvar clock-reminder-interval 300
  "How often to gently remind the user to stay focused.")

(defun local/set-up-clock-notifications ()
  (let ((notif-id nil))
    (named-timer-run
        :clock-reminder t clock-reminder-interval
        (lambda ()
          (if (and (not (-contains-p '(:short-break :long-break) org-pomodoro-state))
                   (org-clocking-p))
              (setq notif-id
                    (notifications-notify
                     :title "Clocked in"
                     :app-name "Org mode"
                     :replaces-id notif-id
                     :timeout 5000
                     :body (format "Working on task \"%s\": %s invested this session."
                                   org-clock-heading
                                   (format-time-string "%H:%M:%S" (time-since org-clock-start-time) t)))))))
    (named-timer-run
        :clock-check t clock-check-interval
        (lambda ()
          (if (and (not (-contains-p '(:short-break :long-break) org-pomodoro-state))
                   (not (org-clocking-p)))
              (setq notif-id
                    (notifications-notify
                     :title "Not clocked in"
                     :app-name "Org mode"
                     :replaces-id notif-id
                     :timeout 5000
                     :body (format "%s since the last clock-out event."
                                   (format-time-string
                                    "%H:%M:%S"
                                    (time-since org-clock-out-time) t)))))))))

(local/set-up-clock-notifications)

(add-hook! 'org-pomodoro-finished-hook
  (defun local/pomodoro/notify-on-finish ()
    (notifications-notify :title "Time for a break"
                          :timeout 5000
                          :body "Pomodoro finished, time for a break.")))

(add-hook! 'org-pomodoro-break-finished-hook
  (defun local/pomodoro/notify-on-break-finish ()
    (notifications-notify :title "Time to get back to work"
                          :timeout 5000
                          :body "Break over, start another pomodoro.")))

(defun org-babel-edit-prep:python (babel-info)
  (setq-local buffer-file-name (->> babel-info caddr (alist-get :tangle)))
  (lsp))

                                        ; (use-package! org-fc
                                        ;   :custom (org-fc-directories (list org-directory))
                                        ;   :config
                                        ;   (require 'org-fc-hydra))

(use-package! vulpea
  :hook ((org-roam-db-autosync-mode . vulpea-db-autosync-enable)))
(use-package! vulpea-buffer)

;; strip the ugly roam timestamp from the agenda
(setq org-agenda-prefix-format
      '((agenda . " %i %-50(local/agenda-prefix)%?-20t% s")
        (todo . " %i %-50(local/agenda-prefix) ")
        (tags . " %i %-50(local/agenda-prefix) ")
        (search . " %i %-50(local/agenda-prefix) ")))

;; Adapted from:
;; https://d12frosted.io/posts/2020-06-24-task-management-with-roam-vol2.html
(defun local/agenda-prefix ()
  "Get category of item at point for agenda.
Refer to `org-agenda-prefix-format' for more information."
  (let* ((file-name (when buffer-file-name
                      (file-name-sans-extension
                       (file-name-nondirectory buffer-file-name))))
         (title (vulpea-buffer-title-get))
         (category (org-get-category))
         (category-pretty (or (if (and
                                   title
                                   (string-equal category file-name))
                                  title
                                ;; there is no title, or the category is not the default one
                                category)
                              ""))
         (breadcrumbs (org-format-outline-path (take 2 (org-get-outline-path)) nil nil " > ")))
    (if (string-empty-p breadcrumbs) category-pretty
      (format "%s: %s" category-pretty breadcrumbs))))

;; (defun org-entry-properties-inherit-deadline (orig-fun &optional pom which)
;;   "Call ORIG-FUN with POM, but if WHICH is `DEADLINE' do it recursively."

;;   (if (or (string= which "DEADLINE") (string= which "SCHEDULED"))
;;       (org-with-point-at pom
;;         (let (value)
;;           (while (not (or (setq value (funcall orig-fun (point) which))
;;                           (not (org-up-heading-safe)))))
;;           value)
;;         (funcall orig-fun pom which))))
;; (advice-add 'org-entry-properties :around #'org-entry-properties-inherit-deadline)
;;

(defun local/reading-speed-table-content ()
  (interactive)
  (let ((table-content
         (save-excursion
           (org-back-to-heading t)
           (save-restriction
             (org-narrow-to-element)
             (let* ((buf (buffer-string))
                    (entries (save-match-data
                               (let ((regexp "CLOCK:.*? =>  \\(.*?\\):\\(..\\)\n.*?Stopped at \\(.*?\\)c.")
                                     (pos 0)
                                     matches)
                                 (while (string-match regexp buf pos)
                                   (let* ((minutes (string-to-number (match-string 2 buf)))
                                          (hours (string-to-number (match-string 1 buf)))
                                          (minutes-taken (+ minutes (* 60 hours)))
                                          (end-pos (string-to-number (match-string 3 buf))))
                                     (push (list end-pos minutes-taken) matches))
                                   (setq pos (match-end 0)))
                                 matches)))
                    (diffs (let ((end-char 0)
                                 (total-time 0))
                             ;; There surprisingly seems to be no inverse to `org-table-to-lisp', so we use
                             ;; `org-table-convert-region' instead.
                             (with-temp-buffer
                               (erase-buffer)
                               (insert "Start pos;End pos;Chars read;Time taken (min);Speed (cph)\n")
                               (mapc #'(lambda (elt)
                                         (let* ((this-end-char (car elt))
                                                (this-min-taken (cadr elt))
                                                (this-char-diff (- this-end-char end-char))
                                                (this-speed (* 60 (/ this-char-diff this-min-taken))))
                                           (insert (format "%d;%d;%d;%d;%d\n"
                                                           end-char
                                                           this-end-char
                                                           this-char-diff
                                                           this-min-taken
                                                           this-speed))
                                           (setq end-char this-end-char)
                                           (setq total-time (+ total-time this-min-taken))))
                                     entries)
                               (let ((net-speed (* 60 (/ end-char total-time))))
                                 (insert (format  ";;%d;%d;%d\n" end-char total-time net-speed)))
                               (org-table-convert-region (point-min) (point-max) ";")
                               ;; Add a header line
                               (goto-char (point-min))
                               (org-ctrl-c-minus)
                               ;; And one for the totals
                               (goto-char (point-max))
                               (forward-line -2)
                               (org-ctrl-c-minus)
                               (s-trim (buffer-string))))))
               diffs)))))
    table-content))

(defun org-dblock-write:reading-speed-table (params)
  (insert (local/reading-speed-table-content)))

(font-lock-add-keywords
 'org-mode
 '(("\{\{\\(.*?\\)::\\(.*?\\)\}\}"
    (0
     (progn
       (put-text-property (match-beginning 0) (match-beginning 2) 'invisible t)
       (put-text-property (match-end 2) (match-end 0) 'invisible t)
       nil))
    (2 font-lock-keyword-face))))

;; (defun local/org-mode-autosave-settings ()
;;   (add-hook 'auto-save-hook 'org-save-all-org-buffers nil nil))
;; (add-hook 'org-mode-hook 'local/org-mode-autosave-settings)

(defmacro local/const (fnc)
  "Return function that ignores its arguments and invokes FNC."
  `(lambda (&rest _rest)
     (funcall ,fnc)))

;; This is nice but the lag is annoying
;; (advice-add 'org-deadline       :after (local/const #'org-save-all-org-buffers))
;; (advice-add 'org-schedule       :after (local/const #'org-save-all-org-buffers))
;; (advice-add 'org-store-log-note :after (local/const #'org-save-all-org-buffers))
;; (advice-add 'org-todo           :after (local/const #'org-save-all-org-buffers))

;; image handling
;; I don't use attachments; as far as I can tell they just don't fucking work because of the stupid buffer-local attachment path setup that I can't be bothered to figure out how to change.
;; org-download also works just fine with screenshots from the clipboard, which is an added advantage.

(use-package! org-download
  :after org
  :config
  (setq org-download-method 'directory)
  (setq-default org-download-image-dir ".attach")
  ;; this is ugly, but required
  ;; the [[download:.*]] links don't respect `org-image-actual-width'
  ;; again, probably fixable, but I can't be bothered
  (setq-default org-download-link-format (format "[[file:%s/%%s]]\n" ".attach")))

(defun local/org-roam-buffer-display-images ()
  "Display images in the Roam buffer.
Smaller previews are better here, since we just want an idea of what's there
without crowding out other backlinks."
  (setq-local org-image-actual-width '(512))
  (org-display-inline-images))

(after! org-roam
  (setq org-roam-buffer-postrender-functions '(local/org-roam-buffer-display-images)))

;; --------------------------------------------------------------------------------
;; ref capture

;; Adapted from org-protocol-capture-html
(defun local/command-line-pandoc-filter (content)
  "Convert input HTML string to org via pandoc."
  (with-temp-buffer
    (insert content)

    (if (not (zerop (call-process-region
                     (point-min) (point-max)
                     "pandoc" t t nil "-f" "html" "-t" "org" "--wrap" "none")))
        (alert (format "Pandoc failed: %s" (buffer-string)) :severity 'high)
      (replace-regexp-in-string (rx "&nbsp;") " " (s-trim (buffer-string)) t t))))

;; no olp because https://github.com/org-roam/org-roam/issues/2429
(use-package! org-roam-protocol
  :after org-roam
  :config
  (setq org-roam-capture-ref-templates
        `(("r" "ref" plain
           ,(format "#+begin_quote\n%%%S\n#+end_quote"
                    '(local/command-line-pandoc-filter (plist-get org-roam-capture--info :body)))
           :target (file+head "%<%Y%m%d%H%M%S>.org" "#+title: ${title}\n")
           :empty-lines 1
           :immediate-finish t
           :jump-to-captured nil)
          ("c" "ref-comment" plain
           ,(format "#+begin_quote\n%%%S\n#+end_quote\n%%?"
                    '(local/command-line-pandoc-filter (plist-get org-roam-capture--info :body)))
           :target (file+head "%<%Y%m%d%H%M%S>.org" "#+title: ${title}\n")
           :empty-lines 1
           :immediate-finish nil
           :jump-to-captured nil))))

(org-link-set-parameters "music-artist")
(org-link-set-parameters "music-album")
(org-link-set-parameters "music-track")

(defun local/org-roam-capture-now-playing ()
  (let* ((metadata (dbus-get-property
                    :session
                    "org.mpris.MediaPlayer2.spotify"
                    "/org/mpris/MediaPlayer2"
                    "org.mpris.MediaPlayer2.Player"
                    "Metadata"))
         (position (dbus-get-property
                    :session
                    "org.mpris.MediaPlayer2.spotify"
                    "/org/mpris/MediaPlayer2"
                    "org.mpris.MediaPlayer2.Player"
                    "Position")))
    (if metadata
        (cl-flet
            ((metadata-get (lambda (k) (caar (alist-get k metadata nil nil 'string-equal))))
             (to-ref (lambda (k) (secure-hash 'md5 k))))
          (let* ((artists (metadata-get "xesam:artist"))
                 (artist (car artists))
                 (artist-qual (format "%s (artist)" artist))
                 (artist-ref (format "music-artist:%s" (to-ref artist)))
                 ;; Rudimentary "typing" for distinctions between artists and albums and tracks (all necessary)
                 (artist-slug (to-ref artist-ref))
                 (album (metadata-get "xesam:album"))
                 (album-qual (format "%s (%s album)" album artist))
                 ;; I could very well concatenate the strings before hashing but I like this more
                 (album-ref (format "music-album:%s:%s" (to-ref artist) (to-ref album)))
                 (album-slug (to-ref album-ref))
                 (track (metadata-get "xesam:title"))
                 (track-qual (format "%s (%s track)" track artist))
                 (track-ref (format "music-track:%s:%s:%s" (to-ref artist) (to-ref album) (to-ref track)))
                 (track-slug (to-ref track-ref))
                 (track-number (metadata-get "xesam:trackNumber"))
                 ;; TODO add this as a ref
                 (track-url (metadata-get "xesam:url"))
                 (artist-node-id nil)
                 (album-node-id nil)
                 (pos (/ position (* 1000 1000)))
                 (pos-seconds (mod pos 60))
                 (pos-total-minutes (/ pos 60))
                 (pos-minutes (mod pos-total-minutes 60))
                 (pos-hours (/ pos-total-minutes 60))
                 (fixup-artist (lambda ()
                                 (org-roam-alias-add artist)))
                 (fixup-album (lambda ()
                                (org-roam-alias-add (format "%s (album)" album))
                                (org-roam-alias-add album)))
                 (fixup-track (lambda ()
                                (org-roam-alias-add (format "%s (track)" track))
                                (org-roam-alias-add track)
                                (org-roam-ref-add track-url))))
            (if (not (org-roam-node-from-ref artist-ref))
                (org-roam-capture-
                 :node (org-roam-node-create :title artist-qual)
                 :templates
                 `(("x" "music-artist" plain ""
                    :target (file+head ,(format "%s.org" artist-slug)
                                       ,(format "#+title: %s\n" artist-qual))
                    :immediate-finish t
                    :jump-to-captured nil
                    :before-finalize (list ,fixup-artist)))
                 :info (list :ref artist-ref :title artist-qual)))
            (setq artist-node-id (org-roam-node-id (org-roam-node-from-ref artist-ref)))
            (if (not (org-roam-node-from-ref album-ref))
                (org-roam-capture-
                 :node (org-roam-node-create :title album-qual)
                 :templates
                 `(("x" "music-album" plain ""
                    :target
                    (file+head
                     ,(format "%s.org" album-slug)
                     ,(format "#+title: %s\n\nAlbum by [[id:%s][%s]]." album-qual artist-node-id artist))
                    :immediate-finish t
                    :jump-to-captured nil
                    :before-finalize (list ,fixup-album)))
                 :info (list :ref album-ref :title album-qual)))
            (setq album-node-id (org-roam-node-id (org-roam-node-from-ref album-ref)))
            (org-roam-capture-
             :node (org-roam-node-create :title track-qual)
             :templates
             `(("x" "music-track" plain
                ,(if (= pos-hours 0)
                     (format "- [%02d:%02d] %%?" pos-minutes pos-seconds)
                   (format "- [%02d:%02d:%02d] %%?" pos-hours pos-minutes pos-seconds))
                :target
                (file+head
                 ,(format "%s.org" track-slug)
                 ,(format "#+title: %s\n\nTrack number %d from [[id:%s][%s]] by [[id:%s][%s]]."
                          track-qual
                          track-number
                          album-node-id album artist-node-id artist))
                :unnarrowed t
                :empty-lines 1
                :immediate-finish nil
                :jump-to-captured nil
                :before-finalize (list ,fixup-track)))
             :info (list :ref track-ref :title track-qual)))))))

;; --------------------------------------------------------------------------------
;; cache which files have TODO headings to make the agenda faster
;; https://d12frosted.io/posts/2021-01-16-task-management-with-roam-vol5.html
(defun local/project-p ()
  "Return non-nil if current buffer has any TODOs or clock logs."
  (let ((els (org-element-parse-buffer 'object))
        (has-clock-drawer
         (lambda (h)
           (string= (org-element-property :drawer-name h)
                    (org-clock-drawer-name))))
        (is-todo
         (lambda (h)
           (let ((todo-type (org-element-property :todo-type h)))
             (or (eq todo-type 'todo)
                 (eq todo-type 'done))))))
    (or (org-element-map els 'headline is-todo nil 'first-match))))

(add-hook 'find-file-hook #'local/project-update-tag)
(add-hook 'before-save-hook #'local/project-update-tag)

(defun local/project-update-tag ()
  "Update PROJECT tag in the current buffer."
  (when (and (not (active-minibuffer-window))
             (local/buffer-p))
    (save-excursion
      (goto-char (point-min))
      (let* ((tags (vulpea-buffer-tags-get))
             (original-tags tags))
        (if (local/project-p)
            (setq tags (cons "project" tags))
          (setq tags (remove "project" tags)))

        ;; cleanup duplicates
        (setq tags (seq-uniq tags))

        ;; update tags if changed
        (when (or (seq-difference tags original-tags)
                  (seq-difference original-tags tags))
          (apply #'vulpea-buffer-tags-set tags)
          (save-buffer))))))

(defun local/buffer-p ()
  "Return non-nil if the currently visited buffer is a note."
  (and buffer-file-name
       (string-prefix-p
        (expand-file-name (file-name-as-directory org-roam-directory))
        (file-name-directory buffer-file-name))))

;; When half of the agenda is missing, run a force-sync (C-u M-x org-roam-db-sync)
;; and then refresh the agenda file list. For why, look at the body of this function.
(defun local/project-files ()
  "Return a list of note files containing PROJECT tag."
  (seq-uniq
   (seq-map
    #'car
    (org-roam-db-query
     [:select [nodes:file]
      :from tags
      :left-join nodes
      :on (= tags:node-id nodes:id)
      :where (like tag (quote "%\"project\"%"))]))))

(defun local/agenda-files-update (&rest _)
  "Update the value of `org-agenda-files'."
  (setq org-agenda-files (local/project-files)))

(advice-add 'org-agenda :before #'local/agenda-files-update)
(advice-add 'org-todo-list :before #'local/agenda-files-update)

(defun local/refresh-agenda-files ()
  "Update the list of agenda files by refreshing the org-roam database."
  (interactive)
  ;; (org-roam-db-sync)
  (local/agenda-files-update))

(defun local/recompute-agenda-file-tags ()
  "Fix missing :PROJECT: tags on agenda files. Warning: slow as shit.
TODO maybe force org to be 'minimal' when loading these files into buffers."
  (interactive)
  (let* ((files (org-roam-list-files))
         (count (length files)))
    (-each-indexed files
      (lambda (ix file)
        (message "processing file %d/%d: %s" ix count file)
        (with-current-buffer (or (find-buffer-visiting file)
                                 (find-file-noselect file))
          (local/project-update-tag)
          (save-buffer))))))

(add-hook! 'org-mode-hook
           #'prettify-symbols-mode)
(setq-hook! org-mode
  prettify-symbols-alist '(("#+end_quote" . "”")
                           ("#+END_QUOTE" . "”")
                           ("#+begin_quote" . "“")
                           ("#+BEGIN_QUOTE" . "“")
                           ("#+begin_src" . "«")
                           ("#+BEGIN_SRC" . "«")
                           ("#+end_src" . "»")
                           ("#+END_SRC" . "»")
                           ("#+name:" . "»")
                           ("#+NAME:" . "»")))

(setq org-superstar-item-bullet-alist '((?* . ?⋆)
                                        (?+ . ?‣)
                                        (?- . ?•)))
(setq org-ellipsis " ▾")
(setq org-startup-with-inline-images t)
(setq org-image-actual-width 600)

(map!
 :leader
 :prefix "n q"
 :desc "find in agenda" :g "f" #'org-ql-find-in-agenda)
