;;; init-core.el --- Core Emacs Configurations -*- lexical-binding: t -*-

;;; Code:

;; Default Emacs Configurations
(use-package emacs
  :ensure nil
  :config
  ;; Welcome message
  (add-hook 'after-init-hook
            (lambda ()
              (require 'server)
              (unless (server-running-p)
                (server-start))
              (message "Emacs has fully loaded.")
              (with-current-buffer (get-buffer-create "*scratch*")
                (insert (format ";;    Welcome to Emacs!
;;
;;    Loading time : %s
;;    Packages     : %s
"
                                (emacs-init-time)
                                (number-to-string (length package-activated-list)))))))

  ;; Custom file setup
  (setq custom-file (locate-user-emacs-file "custom-vars.el"))
  (load custom-file 'noerror 'nomessage)

  :config
  (defun skip-these-buffers (_window buffer _bury-or-kill)
    "Skip buffers matching the pattern when switching."
    (string-match "\\*[^*]+\\*" (buffer-name buffer)))
  (setq switch-to-prev-buffer-skip 'skip-these-buffers)

  ;; Advice for completing-read-multiple
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Make Escape quit prompts
  (global-set-key [escape] 'keyboard-escape-quit)
  (global-set-key (kbd "C-+") 'text-scale-increase)
  (global-set-key (kbd "C--") 'text-scale-decrease)
  (global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
  (global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

  ;; Enable global-auto-revert-mode
  (global-auto-revert-mode 1)

  :hook
  (prog-mode . display-line-numbers-mode)
  (emacs-lisp-mode . hs-minor-mode)
  )

(use-package eldoc
  :ensure nil
  :init
  (global-eldoc-mode))

(use-package vc
  :ensure nil                        ;; This is built-in, no need to fetch it.
  :defer t
  :bind
  (("C-x v d" . vc-dir)              ;; Open VC directory for version control status.
   ("C-x v =" . vc-diff)             ;; Show differences for the current file.
   ("C-x v D" . vc-root-diff)        ;; Show differences for the entire repository.
   ("C-x v v" . vc-next-action))     ;; Perform the next version control action.
  :config
  (setq vc-annotate-color-map
        '((20 . "#f5e0dc")
          (40 . "#f2cdcd")
          (60 . "#f5c2e7")
          (80 . "#cba6f7")
          (100 . "#f38ba8")
          (120 . "#eba0ac")
          (140 . "#fab387")
          (160 . "#f9e2af")
          (180 . "#a6e3a1")
          (200 . "#94e2d5")
          (220 . "#89dceb")
          (240 . "#74c7ec")
          (260 . "#89b4fa")
          (280 . "#b4befe"))))

(use-package ibuffer
  :ensure nil ; It's a built-in package
  :config
  (setq ibuffer-show-empty-filter-groups nil
        ibuffer-filter-group-name-face '(:inherit (success bold))
        ibuffer-formats
        `((mark modified read-only locked
                " " (all-the-icons 4 4 :left)
                (name 18 18 :left :elide)
                " " (size 9 -1 :right)
                " " (mode 16 16 :left :elide)
                ,@(when (require 'ibuffer-vc nil t)
                    '(" " (vc-status 12 :left)))
                " " filename-and-process)
          (mark " " (name 16 -1) " " filename)))

  (define-ibuffer-column size (:name "Size" :inline t)
    (file-size-human-readable (buffer-size)))

  ;; (evil-define-key 'normal ibuffer-mode-map "q" #'kill-current-buffer)
  )

(use-package ibuffer-vc
  :ensure t)

(use-package dired
  :ensure nil                                                ;; This is built-in, no need to fetch it.
  :custom
  (dired-listing-switches "-lah -v --group-directories-first")  ;; Display files in a human-readable format and group directories first.
  (dired-dwim-target t)                                      ;; Enable "do what I mean" for target directories.
  (dired-guess-shell-alist-user
   '(("\\.\\(png\\|jpe?g\\|tiff\\)" "feh" "xdg-open" "open") ;; Open image files with `feh' or the default viewer.
     ("\\.\\(mp[34]\\|m4a\\|ogg\\|flac\\|webm\\|mkv\\)" "mpv" "xdg-open" "open") ;; Open audio and video files with `mpv'.
     (".*" "open" "xdg-open")))                              ;; Default opening command for other files.
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-auto-revert-buffer #'dired-buffer-stale-p)
  (dired-recursive-copies  'always)
  (dired-recursive-deletes 'top)
  (dired-create-destination-dirs 'ask)
  (image-dired-dir (concat doom-cache-dir "image-dired/"))
  (image-dired-db-file (concat image-dired-dir "db.el"))
  (image-dired-gallery-dir (concat image-dired-dir "gallery/"))
  (image-dired-temp-image-file (concat image-dired-dir "temp-image"))
  (image-dired-temp-rotate-image-file (concat image-dired-dir "temp-rotate-image"))
  (image-dired-thumb-size 150)
  :config
  (when (eq system-type 'darwin)
    (let ((gls (executable-find "gls")))
      (when gls
	(setq insert-directory-program gls))))
  (add-hook 'dired-mode-hook
            (lambda ()
              (define-key dired-mode-map (kbd "-") #'dired-up-directory)))
  )

(use-package diredfl
  :after dired
  :hook (dired-mode . diredfl-mode))

(use-package which-key
  :init
  (which-key-mode 1)
  :diminish
  :custom
  (which-key-side-window-location 'bottom)
  (which-key-sort-order #'which-key-key-order-alpha) ;; Same as default, except single characters are sorted alphabetically
  (which-key-sort-uppercase-first nil)
  (which-key-add-column-padding 1)
  (which-key-min-display-lines 6)
  (which-key-idle-delay 1.0)
  (which-key-max-description-length 25)
  (which-key-allow-imprecise-window-fit nil)) ;; Fixes which-key window slipping out in Emacs Daemon

(use-package window
  :ensure nil
  :custom
  (display-buffer-alist
   '(
	 ;; ("\\*.*e?shell\\*"
     ;;  (display-buffer-in-side-window)
     ;;  (window-height . 0.25)
     ;;  (side . bottom)
     ;;  (slot . -1))

     ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|[Hh]elp\\|Messages\\|Bookmark List\\|Ibuffer\\|Occur\\|eldoc.*\\)\\*"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 0))

     ("\\*\\(lsp-help\\)\\*"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 0))

     ("\\*\\(Flymake diagnostics\\|xref\\|ivy\\|Swiper\\|Completions\\)"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 1))
	 )))

(use-package isearch
  :ensure nil
  :config
  (setq isearch-lazy-count t)                  ;; Enable lazy counting to show current match information.
  (setq lazy-count-prefix-format "(%s/%s) ")   ;; Format for displaying current match count.
  (setq lazy-count-suffix-format nil)          ;; Disable suffix formatting for match count.
  (setq search-whitespace-regexp ".*?")        ;; Allow searching across whitespace.
  :bind (("C-s" . isearch-forward)             ;; Bind C-s to forward isearch.
         ("C-r" . isearch-backward)))          ;; Bind C-r to backward isearch.

(use-package recentf
  :ensure nil
  :config
  (recentf-mode 1)
  (setq recentf-max-saved-items 200) ;; Increase the limit
  (setq recentf-exclude '("/tmp/" "/ssh:"))) ;; Exclude noise

;; Temporary fix for hack-dir-local-variables error in consult
(defun my-suppress-dir-locals-error (orig-fun &rest args)
  "Suppress errors in `hack-dir-local-variables`."
  (ignore-errors (apply orig-fun args)))
(advice-add 'hack-dir-local-variables :around #'my-suppress-dir-locals-error)

(provide 'init-core)
;;; init-core.el ends here
