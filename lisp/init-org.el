;;; init-org.el --- Org-Mode and Denote -*- lexical-binding: t -*-

;;; Code:

;;-----------------------------------------------------------------------------
;; Org-Mode
;;-----------------------------------------------------------------------------
(use-package org
  :ensure t
  :hook
  (org-mode . org-indent-mode)
  (org-mode . visual-line-mode)
  :custom
  (org-return-follows-link t)
  :config
  (setq org-startup-with-inline-images t)
  (setq org-image-actual-width 500)
  (setq org-directory "~/Dropbox/org/")
  (setq org-todo-keywords
		'((sequence "TODO(t)" "DOING(d!)" "HOLD(h)" "|" "DONE(D)" "CANCELLED(c)" "MAYBE(m)")))
  (setq org-todo-keyword-faces
		'(("DOING" . (:background "orange" :foreground "black"))
		  ("DONE" . (:background "green" :foreground "black"))
		  ("HOLD" . (:background "turquoise" :foreground "black"))
		  ("TODO" . (:background "red" :foreground "white"))
		  ("CANCELLED" . (:background "gray" :foreground "black"))
		  ("MAYBE" . (:background "yellow" :foreground "black"))))
  (setq org-agenda-prefix-format
		'((agenda . " %i %?-12t% s")
		  (todo . " %i ")
		  (tags . " %i ")
		  (search . " %i ")))
  (setq org-log-done 'time)
  (setq org-default-notes-file (concat org-directory "/inbox.org"))
  (setq org-attach-id-dir "~/Dropbox/org/assets/")
  (setq org-attach-use-inheritance t)
  (setq org-capture-templates
		'(("t" "Blank Todo [inbox]" entry
		   (file+headline "~/Dropbox/org/inbox.org" "Tasks")
		   "* TODO %i%?")
		  ("w" "Work Todo [work]" entry
		   (file+headline "~/Dropbox/org/work.org" "Work")
		   "* TODO %i%?")
		  ("p" "Personal Todo [personal]" entry
		   (file+headline "~/Dropbox/org/personal.org" "Personal")
		   "* TODO %i%?")))
  (setq org-hide-emphasis-markers t)
  )

(defun my-org-agenda-files-recursive (directory)
  "Recursively find all .org files in DIRECTORY."
  (directory-files-recursively directory "\\.org$"))
(defun my-update-org-agenda-files (&rest _)
  "Update `org-agenda-files` to include all .org files in the directory."
  (setq org-agenda-files (my-org-agenda-files-recursive "~/Dropbox/org/")))

(advice-add 'org-agenda :before #'my-update-org-agenda-files)

(defun my-kill-agenda-file-buffers ()
  "Kill unmodified buffers visiting files listed in `org-agenda-files`."
  (interactive)
  (let ((killed-count 0)
        (agenda-files (org-agenda-files t))
        (agenda-files-set (make-hash-table :test 'equal)))
    (dolist (file agenda-files)
      (puthash (expand-file-name file) t agenda-files-set))

    (dolist (buffer (buffer-list))
      (let ((filename (buffer-file-name buffer)))
        (when (and filename
                   (gethash (expand-file-name filename) agenda-files-set)
                   (not (buffer-modified-p buffer))
                   (not (eq buffer (current-buffer)))
                   (not (string-prefix-p "*" (buffer-name buffer))))
          (kill-buffer buffer)
          (setq killed-count (1+ killed-count)))))
    (message "Killed %d agenda file buffers." killed-count)))

(defface my-note-face
  '((t :background "#a6d189" :foreground "#1e1e2e" :extend t))
  "Face for NOTE blocks with full width background.")
(defface my-important-face
  '((t :background "#ea6962" :foreground "#1e1e2e" :extend t))
  "Face for IMPORTANT blocks with full width background.")
(defface my-warning-face
  '((t :background "#e5c890" :foreground "#1e1e2e" :extend t))
  "Face for WARNING blocks with full width background.")
(defun apply-custom-org-block-face (start end face)
  "Apply FACE to the entire visual line from START to END."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face face)
    (overlay-put overlay 'line-prefix
                 nil)
    (overlay-put overlay 'after-string
                 (propertize " " 'face face 'display '(space :align-to right-margin)))))
(defun highlight-org-block-region ()
  "Highlight org blocks with full window width background."
  (remove-overlays (point-min) (point-max))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*#\\+BEGIN_\\(NOTE\\|IMPORTANT\\|WARN\\)" nil t)
      (let* ((block-type (match-string 1))
             (block-start (line-beginning-position))
             (face (pcase block-type
                     ("NOTE" 'my-note-face)
                     ("IMPORTANT" 'my-important-face)
                     ("WARN" 'my-warning-face))))
        (when (re-search-forward (format "^[ \t]*#\\+END_%s" block-type) nil t)
          (apply-custom-org-block-face block-start (line-end-position) face))))))
(define-minor-mode org-block-highlight-mode
  "Minor mode for highlighting org blocks with full window width background."
  :lighter " OrgBlockHL"
  (if org-block-highlight-mode
      (progn
        (highlight-org-block-region)
        (add-hook 'after-change-functions
                  (lambda (&rest _) (highlight-org-block-region))
                  nil t))
    (remove-overlays (point-min) (point-max))))
(add-hook 'org-mode-hook 'org-block-highlight-mode)

(font-lock-mode 1)

(defun my-org-faces ()
  (set-face-attribute 'org-document-title nil :height 2.0 :family "Iosevka Aile")
  (set-face-attribute 'org-level-1 nil :height 1.8 :family "Iosevka Aile")
  (set-face-attribute 'org-level-2 nil :height 1.6 :family "Iosevka Aile")
  (set-face-attribute 'org-level-3 nil :height 1.4 :family "Iosevka Aile")
  (set-face-attribute 'org-level-4 nil :height 1.2 :family "Iosevka Aile")
  (set-face-attribute 'org-level-5 nil :height 1.1 :family "Iosevka Aile")
  (set-face-attribute 'org-block nil :height 1.0 :family "IosevkaTerm Nerd Font")
  (set-face-attribute 'org-code nil :height 1.0 :family "IosevkaTerm Nerd Font")
  (set-face-attribute 'org-table nil :height 1.0 :family "IosevkaTerm Nerd Font")
  )
(add-hook 'org-mode-hook #'my-org-faces)
;; (add-hook 'org-mode-hook #'variable-pitch-mode)

(use-package denote
  :ensure t
  :hook (dired-mode . denote-dired-mode)
  :config
  (setq denote-directory (expand-file-name "~/Dropbox/org/pages/"))
  (denote-rename-buffer-mode 1))

(use-package denote-org
  :ensure t)

(use-package toc-org
  :commands toc-org-enable
  :hook (org-mode . toc-org-mode))

(use-package org-tempo
  :ensure nil
  :after org)

(use-package org-modern
  :after org
  :config
  (setq org-modern-star nil)

  (setq org-agenda-tags-column 0)
  (setq org-agenda-block-separator ?─)
  (setq org-agenda-time-grid
        '((daily today require-timed)
          (800 1000 1200 1400 1600 1800 2000)
          " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (setq org-agenda-current-time-string
        "<-- now ───────")
  (setq org-modern-priority nil)

  (setq org-modern-todo-faces
        (quote (("TODO" :background "red" :foreground "white")
                ("DOING" :background "orange" :foreground "black")
                ("HOLD" :background "turquoise" :foreground "black")
                ("CANCELLED" . (:background "gray" :foreground "black"))
                ("MAYBE" . (:background "yellow" :foreground "black"))
                ("DONE" . (:background "green" :foreground "black")))))
  (global-org-modern-mode)
  )

(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode -1)))

(provide 'init-org)
;;; init-org.el ends here
