;;; config.magit.el -*- lexical-binding: t; -*-

;; Refresh the magit status buffer after any edits performed within Emacs.
;; Note that this does not set file watchers on the files in the repo or
;; anything similar, so changes e.g. made by a formatter will not be picked up.
(after! magit
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t)

  ;; Adapted from https://github.com/magit/forge/issues/91#issuecomment-1003823864
  (defun local/forge--buffer-file ()
    (let
        ((rev (magit-rev-parse "HEAD"))
         (repo (forge-get-repository :stub))
         (file (magit-file-relative-name buffer-file-name))
         (highlight
          (if
              (use-region-p)
              (let ((l1 (line-number-at-pos (region-beginning)))
                    (l2 (line-number-at-pos (- (region-end) 1))))
                (format "#L%d-L%d" l1 l2))
            (format "#L%d" (line-number-at-pos (point))))))
      (forge--format repo "https://%h/%o/%n/blob/%r/%f%L"
                     `((?r . ,rev) (?f . ,file) (?L . ,highlight)))))

  (defun local/forge-browse-buffer-file ()
    "Open a link to the file in the Git forge with the active region or point
selected."
    (interactive)
    (browse-url (local/forge--buffer-file)))

  (defun local/forge-buffer-file-as-kill ()
    "Copy a link to the file in the Git forge with the active region or point
selected."
    (interactive)
    (kill-new (local/forge--buffer-file)))

  (map! :leader
        :prefix "g"
        :desc "Copy region at point" :n "oy" #'local/forge-buffer-file-as-kill
        :desc "Open region at point" :n "of" #'local/forge-browse-buffer-file))
