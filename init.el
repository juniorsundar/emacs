;;; init.el --- My Emacs Config -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Author: Junior Sundar
;; Version: 0.1.0
;; Package-Requires: ((Emacs "30.0"))
;;
;;; Code:

;;-----------------------------------------------------------------------------
;; Default Emacs Configurations
;;-----------------------------------------------------------------------------
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

  :hook
  (prog-mode . display-line-numbers-mode)
  (emacs-lisp-mode . hs-minor-mode)
  )

(use-package vterm
  :ensure t)

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

;; (use-package ibuffer-projectile
;;   :ensure t
;;   :hook (ibuffer . ibuffer-projectile-set-filter-groups)
;;   :config
;;   (setq ibuffer-projectile-prefix
;;         (if (and (display-graphic-p) (require 'nerd-icons nil t))
;;             (concat (nerd-icons-octicon "nf-oct-file_directory"
;; 										:face 'ibuffer-filter-group-name-face
;; 										:v-adjust -0.05)
;;                     " ")
;;           "Project: ")))

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

(use-package flymake
  :ensure nil
  :defer t
  :hook ((prog-mode text-mode) . flymake-mode)
  :custom
  (flymake-margin-indicators-string
   '((error "!»" compilation-error) (warning "»" compilation-warning)
	 (note "»" compilation-info)))
  (flymake-fringe-indicator-position 'right-fringe)
  )

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
  (which-key-idle-delay 0.0)
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

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; (use-package diminish)

(use-package isearch
  :ensure nil
  :config
  (setq isearch-lazy-count t)                  ;; Enable lazy counting to show current match information.
  (setq lazy-count-prefix-format "(%s/%s) ")   ;; Format for displaying current match count.
  (setq lazy-count-suffix-format nil)          ;; Disable suffix formatting for match count.
  (setq search-whitespace-regexp ".*?")        ;; Allow searching across whitespace.
  :bind (("C-s" . isearch-forward)             ;; Bind C-s to forward isearch.
         ("C-r" . isearch-backward)))          ;; Bind C-r to backward isearch.

(use-package smerge-mode
  :ensure nil
  :defer t
  :bind (:map smerge-mode-map
              ("C-c s u" . smerge-keep-upper)  ;; Keep the changes from the upper version.
              ("C-c s l" . smerge-keep-lower)  ;; Keep the changes from the lower version.
              ("C-c s n" . smerge-next)        ;; Move to the next conflict.
              ("C-c s p" . smerge-previous)))  ;; Move to the previous conflict.
;;-----------------------------------------------------------------------------
;; Evil
;;-----------------------------------------------------------------------------
(use-package avy
  :ensure t)
;;-----------------------------------------------------------------------------
;; Theme
;;-----------------------------------------------------------------------------
(add-to-list 'default-frame-alist '(alpha-background . 100)) ;; For all new frames henceforth

(use-package doom-themes
  :ensure t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  (doom-themes-treemacs-theme "doom-atom")
  :config
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(use-package spacious-padding
  :ensure t
  :if (display-graphic-p)
  :custom
  (spacious-padding-subtle-frame-lines t)
  :config
  (spacious-padding-mode t)
  )
;;-----------------------------------------------------------------------------
;; Fonts
;;-----------------------------------------------------------------------------
(defun fixed-pitch-mode ()
  (interactive)
  (buffer-face-mode -1))
(defun variable-pitch-mode ()
  (interactive)
  (buffer-face-mode t))
(defun toggle-pitch (&optional arg)
  "Switch between the `fixed-pitch' face and the `variable-pitch' face"
  (interactive)
  (buffer-face-toggle 'variable-pitch))
(buffer-face-mode)

(add-hook 'eww-mode-hook 'variable-pitch-mode)

(defun my-set-frame-fonts (frame)
  "Set fonts for the given FRAME."
  (when (display-graphic-p frame)
    (with-selected-frame frame
      (set-face-font 'default "IosevkaTerm Nerd Font")
      (set-face-font 'variable-pitch "Iosevka Aile")
      (copy-face 'default 'fixed-pitch)

      (set-face-attribute 'default nil :height 130)
      (set-face-attribute 'variable-pitch nil :height 130)

      (buffer-face-mode)

      (when (member "Noto Color Emoji" (font-family-list))
        (set-fontset-font
         t 'symbol (font-spec :family "Noto Color Emoji") nil 'prepend))

      (set-face-attribute 'italic nil
                          :underline nil
                          :slant 'italic
                          :family "IosevkaTerm Nerd Font")
      )))

(add-hook 'after-make-frame-functions #'my-set-frame-fonts)
(when (and (not (daemonp)) (display-graphic-p))
  (my-set-frame-fonts (selected-frame)))

;; Set Nerd Font for symbols
(when (member "Noto Color Emoji" (font-family-list))
  (set-fontset-font
   t 'symbol (font-spec :family "Noto Color Emoji") nil 'prepend))

(set-face-attribute 'italic nil
                    :underline nil
                    :slant 'italic
                    :family "IosevkaTerm Nerd Font")

(add-hook 'ibuffer-mode-hook (lambda () (display-line-numbers-mode -1)))

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p)
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package all-the-icons-dired
  :ensure t)

(use-package all-the-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode))

(use-package nerd-icons-corfu
  :ensure t
  :after (:all corfu))

(use-package ligature
  :config
  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  (ligature-set-ligatures 't
			  '(("=" (rx (+ (or ">" "<" "|" "/" "~" ":" "!" "="))))
			    (";" (rx (+ ";")))
			    ("&" (rx (+ "&")))
			    ("!" (rx (+ (or "=" "!" "\." ":" "~"))))
			    ("?" (rx (or ":" "=" "\." (+ "?"))))
			    ("%" (rx (+ "%")))
			    ("|" (rx (+ (or ">" "<" "|" "/" ":" "!" "}" "\]"
					    "-" "=" ))))
			    ("\\" (rx (or "/" (+ "\\"))))
			    ("+" (rx (or ">" (+ "+"))))
			    (":" (rx (or ">" "<" "=" "//" ":=" (+ ":"))))
			    ("/" (rx (+ (or ">"  "<" "|" "/" "\\" "\*" ":" "!"
					    "="))))
			    ("\." (rx (or "=" "-" "\?" "\.=" "\.<" (+ "\."))))
			    ("-" (rx (+ (or ">" "<" "|" "~" "-"))))
			    ("*" (rx (or ">" "/" ")" (+ "*"))))
			    ("w" (rx (+ "w")))
			    ("<" (rx (+ (or "\+" "\*" "\$" "<" ">" ":" "~"  "!"
					    "-"  "/" "|" "="))))
			    (">" (rx (+ (or ">" "<" "|" "/" ":" "=" "-"))))
			    ("#" (rx (or ":" "=" "!" "(" "\?" "\[" "{" "_(" "_"
					 (+ "#"))))
			    ("~" (rx (or ">" "=" "-" "@" "~>" (+ "~"))))
			    ("_" (rx (+ (or "_" "|"))))
			    ("0" (rx (and "x" (+ (in "A-F" "a-f" "0-9")))))
			    "Fl"  "Tl"  "fi"  "fj"  "fl"  "ft"
			    "{|"  "[|"  "]#"  "(*"  "}#"  "$>"  "^="))
  (global-ligature-mode t))

;;-----------------------------------------------------------------------------
;; Modeline
;;-----------------------------------------------------------------------------
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 25)
  (doom-modeline-bar-width 0.1)
  (doom-modeline-buffer-name t)
  (doom-modeline-vcs-max-length 25)
  (doom-modeline-persp-name t)
  (doom-modeline-persp-icon t)
  (doom-modeline-modal nil)
  (doom-modeline-percent-position nil)
  (doom-modeline-buffer-file-name-style 'buffer-name)
  (doom-modeline-project-detection 'project)
  (doom-modeline-icon t)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-enable-word-count t)
  )

;;-----------------------------------------------------------------------------
;; Projectile
;;-----------------------------------------------------------------------------
;; (use-package projectile
;;   :init
;;   (projectile-mode)
;;   :custom
;;   (projectile-run-use-comint-mode t)
;;   (projectile-switch-project-action #'projectile-dired)
;;   (projectile-project-search-path '("~/Documents/Projects/"
;; 									;; "~/Documents/work/"
;; 									)
;; 								  )
;;   )

;;-----------------------------------------------------------------------------
;; LSP and Language Modes
;;-----------------------------------------------------------------------------
(use-package direnv
  :config
  (direnv-mode))

(use-package eglot
  :ensure nil
  :hook ((go-ts-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)
         (zig-ts-mode . eglot-ensure)
         (rust-ts-mode . eglot-ensure)
		 )
  :custom
  (eglot-autoshutdown t)
  (eldoc-echo-area-use-multiline-p nil)
  (eglot-code-action-suggestion nil)
  :config
  (add-hook 'eglot-managed-mode-hook (lambda () (eglot-inlay-hints-mode -1)))
  (setq eglot-inlay-hints-mode nil)
  (setq eglot-code-action-indicator "")
  (setq eglot-code-action-indications '(mode-line))
  )

(defvar-local my--eldoc-buffer-tracker nil
  "Buffer-local variable to track if this buffer showed eldoc docs.")
(advice-add 'eldoc--handle-doc-buffer :after
            (lambda (&rest _)
              (when (get-buffer-window "*eldoc*")
                (with-current-buffer "*eldoc*"
                  (setq my--eldoc-buffer-tracker t)))))
(defun my-close-eldoc-buffer-if-left ()
  "Close *eldoc* buffer if point is no longer in it."
  (unless (or (null (get-buffer "*eldoc*"))
              (eq (current-buffer) (get-buffer "*eldoc*")))
    (let ((win (get-buffer-window "*eldoc*")))
      (when win
        (quit-window t win)))))
(add-hook 'post-command-hook #'my-close-eldoc-buffer-if-left)

(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode)
  :config
  ;; You can also use (yas-global-mode 1) here instead of the hook
  ;; if you want it on in *all* buffers, including non-prog-mode.
  (message "YASnippet engine loaded."))

;; This package just provides the snippet collection
(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (setq treesit-auto-add-to-auto-mode-alist 'all)
  (setq global-treesit-auto-mode t))

;;-----------------------------------------------------------------------------
;; Language Modes
;;-----------------------------------------------------------------------------
(use-package markdown-mode)
(add-to-list 'major-mode-remap-alist '(markdown-mode . markdown-ts-mode))
;; (add-hook 'markdown-ts-mode-hook 'variable-pitch-mode)
(add-hook 'markdown-ts-mode-hook 'visual-line-mode)
(add-hook 'markdown-ts-mode-hook (lambda () (display-line-numbers-mode -1)))

(use-package python-mode)
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
(require 'python)

(use-package go-mode)
(add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode))

(use-package zig-mode)
(add-to-list 'major-mode-remap-alist '(zig-mode . zig-ts-mode))

(use-package rust-mode)
(add-to-list 'major-mode-remap-alist '(rust-mode . rust-ts-mode))

(use-package nix-mode)
(add-to-list 'major-mode-remap-alist '(nix-mode . nix-ts-mode))

(use-package lua-mode)
(add-to-list 'major-mode-remap-alist '(lua-mode . lua-ts-mode))
;;-----------------------------------------------------------------------------
;; Git Integration
;;-----------------------------------------------------------------------------
(use-package transient
  :ensure t)
(use-package magit
  :ensure t
  :after transient
  :commands (magit-status magit-blame-addition)
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1
        magit-bury-buffer-function #'magit-mode-quit-window))

(use-package diff-hl
  :defer t
  :ensure t
  :hook ((find-file . diff-hl-mode)
         (after-save . diff-hl-update)
         (vc-dir-mode . diff-hl-dir-mode))
  :init
  (global-diff-hl-mode 1)
  :config
  (diff-hl-flydiff-mode 1)
  (diff-hl-margin-mode 1)
  (add-hook 'diff-hl-mode-hook
            (lambda ()
              (set-face-background 'diff-hl-insert nil)
              (set-face-background 'diff-hl-delete nil)
              (set-face-background 'diff-hl-change nil)))
  :custom
  (diff-hl-side 'left)
  (diff-hl-margin-symbols-alist '((insert . "┃")
                                  (delete . "-")
                                  (change . "┃")
                                  (unknown . "?")
                                  (ignored . "i"))))
;; -----------------------------------------------------------------------------
;; Completions
;;-----------------------------------------------------------------------------
(use-package corfu
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-prefix 1)          ;; Minimum length of prefix for auto completion.
  (corfu-separator ?\s)          ;; Orderless field separator, Use M-SPC to enter separator
  (corfu-quit-at-boundary 'separator)   ;; Never quit at completion boundary
  (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-popupinfo-mode t)       ;; Enable popup information
  (corfu-popupinfo-delay 0.5)    ;; Lower popupinfo delay to 0.5 seconds from 2 seconds
  (completion-ignore-case t)
  :init
  (global-corfu-mode))

(use-package cape
  :after corfu
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev) ;; Complete word from current buffers
  ;;(add-to-list 'completion-at-point-functions #'cape-dict) ;; Dictionary completion
  (add-to-list 'completion-at-point-functions #'cape-file) ;; Path completion
  (add-to-list 'completion-at-point-functions #'cape-elisp-block) ;; Complete elisp in Org or Markdown mode
  (add-to-list 'completion-at-point-functions #'cape-keyword) ;; Keyword/Snipet completion

  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev) ;; Complete abbreviation
  ;;(add-to-list 'completion-at-point-functions #'cape-history) ;; Complete from Eshell, Comint or minibuffer history
  ;;(add-to-list 'completion-at-point-functions #'cape-line) ;; Complete entire line from current buffer
  ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol) ;; Complete Elisp symbol
  ;;(add-to-list 'completion-at-point-functions #'cape-tex) ;; Complete Unicode char from TeX command, e.g. \hbar
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml) ;; Complete Unicode char from SGML entity, e.g., &alpha
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345) ;; Complete Unicode char using RFC 1345 mnemonics
  )

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package vertico
  :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  (vertico-count 17)
  (vertico-resize t)
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package marginalia
  :after vertico
  :bind (:map minibuffer-local-map
			  ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;;-----------------------------------------------------------------------------
;; Search
;;-----------------------------------------------------------------------------
(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5
		register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
		xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-fd
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<")
  (setq consult-buffer-sources
		'(consult--source-project-buffer ; 1. Show project buffers first
		  consult--source-buffer         ; 2. Then show all other buffers
		  consult--source-recent-file
		  consult--source-bookmark))
  (advice-add #'consult--buffer-filter :override
              (lambda (buffer) (not (consult--project-buffer-p buffer))))
  (setq consult-fd-args
		'((if (executable-find "fdfind" 'remote) "fdfind" "fd")
		  "--color=never"
		  ;; https://github.com/sharkdp/fd/issues/839
		  "--hidden --exclude .git"
		  (if (featurep :system 'windows) "--path-separator=/")))
  )

(use-package embark
  :ensure t
  :bind
  (("C-q" . embark-act)
   ("M-d" . embark-dwim)
   ("C-h B" . embark-bindings))
  )

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package wgrep
  :ensure t)

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

;; (use-package org-roam
;;   :ensure t
;;   :custom
;;   (org-roam-directory (file-truename "~/Dropbox/org/pages"))
;;   (org-roam-dailies-directory "./journals/")
;;   (org-roam-capture-templates
;;    '(("d" "default" plain "%?"
;; 	  :target (file+head "pages/${slug}.org" "#+title: ${title}\n")
;; 	  :unnarrowed t))
;;    org-roam-dailies-capture-templates
;;    '(("d" "default" entry "* %?"
;; 	  :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n#+FILETAGS: journal"))))

;;   :config
;;   (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
;;   (org-roam-db-autosync-mode)
;;   (require 'org-roam-protocol))

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

;;-----------------------------------------------------------------------------
;; Meow
;;-----------------------------------------------------------------------------
(use-package evil
  :init
  (evil-mode)
  :config
  (evil-set-initial-state 'eat-mode 'insert)
  :custom
  (evil-want-keybinding nil)    ;; Disable evil bindings in other modes (It's not consistent and not good)
  (evil-want-C-u-scroll t)      ;; Set C-u to scroll up
  (evil-want-C-i-jump nil)      ;; Disables C-i jump
  (evil-undo-system 'undo-redo) ;; C-r to redo
  ;; Unmap keys in 'evil-maps. If not done, org-return-follows-link will not work
  :bind (:map evil-motion-state-map
			  ("SPC" . nil)
			  ("RET" . nil)
			  ("TAB" . nil)
              ;; Window Management
              ;; ("C-M-<up>" . evil-window-increase-height)
              ;; ("C-M-k" . evil-window-increase-height)
              ;; ("C-M-<down>" . evil-window-decrease-height)
              ;; ("C-M-j" . evil-window-decrease-height)
              ;; ("C-M-<right>" . evil-window-increase-width)
              ;; ("C-M-l" . evil-window-increase-width)
              ;; ("C-M-<left>" . evil-window-decrease-width)
              ;; ("C-M-h" . evil-window-decrease-width)
              )
  ;; (:map evil-visual-state-map
  ;; 		("M-h"      . evil-shift-left)
  ;; 		("M-l"      . evil-shift-right)
  ;; 		("M-<left>" . evil-shift-left)
  ;; 	("M-<right>". evil-shift-right)
  ;; 		)
  )

(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-commentary
  :after evil
  :config (evil-commentary-mode))

(use-package evil-avy
  :after (evil avy)
  :config
  (evil-define-key '(normal visual) 'global "s" #'avy-goto-word-1))


(use-package multiple-cursors
  :ensure t
  :config
  (require 'multiple-cursors)
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

;;-----------------------------------------------------------------------------
;; General Keybindings
;;-----------------------------------------------------------------------------
(use-package general
  :ensure t
  :config
  (require 'general)
  (general-def
    :prefix "C-c"
    :non-normal-prefix "C-c" 
    "." '(find-file :which-key "Find")
    "P" '(projectile-command-map :which-key "Projectile")
    "-" '((lambda () (interactive) (dired default-directory)) :which-key "Dired File")
    "C-s" '(avy-goto-char :which-key "Avy Char")
    "C-j" '(avy-goto-line-below :which-key "Avy Line Below")
    "C-k" '(avy-goto-line-above :which-key "Avy Line Above")
    "C-<up>" '(avy-goto-line-above :which-key "Avy Line Above")
    "C-<down>" '(avy-goto-line-below :which-key "Avy Line Below")
    )

  ;; Define the "Z" (Hide-Show) submap under the C-c leader
  (general-def
    :prefix "C-c z" ; Prefix for hide-show commands
    "a" '(hs-toggle-hiding :which-key "hs Toggle")
    "c" '(hs-hide-block :which-key "hs Hide")
    "o" '(hs-show-block :which-key "hs Show")
    "R" '(hs-show-all :which-key "hs Show All")
    "M" '(hs-hide-all :which-key "hs Hide All")
    )

  ;; Define the "F" (Find) submap under the C-c leader
  (general-def
    :prefix "C-c F" ; Prefix for find commands
    "c" '((lambda () (interactive) (find-file "~/.config/emacs/init.el")) :which-key "Emacs Config")
    "r" '(consult-recent-file :which-key "Recent File (Consult)")
    "f" '(consult-fd :which-key "Fd File (Consult)")
    "t" '(consult-ripgrep :which-key "Rg Text (Consult)")
    "l" '(consult-line :which-key "Find Line (Consult)")
    )

  ;; Define the "B" (Buffer Bookmarks) submap under the C-c leader
  (general-def
    :prefix "C-c B" ; Prefix for buffer/bookmark commands
    "b" '(consult-buffer :which-key "Switch Buffer (Consult)")
    "k" '(kill-this-buffer :which-key "Kill Buffer")
    "i" '(ibuffer :which-key "IBuffer")
    "n" '(next-buffer :which-key "Next Buffer")
    "p" '(previous-buffer :which-key "Previous Buffer")
    "r" '(revert-buffer :which-key "Revert Buffer")
    "j" '(consult-bookmark :which-key "Bookmarks (Consult")
    )

  ;; Define the "G" (Git) submap under the C-c leader
  (general-def
    :prefix "C-c G" ; Prefix for git commands
    "g" '(magit-status :which-key "Magit Status")
    "l" '(magit-log-current :which-key "Log Current (Magit)")
    "d" '(magit-diff-buffer-file :which-key "Diff Buffer (Magit)")
    "p" '(diff-hl-show-hunk :which-key "Show Hunk (diffhl)")
    "s" '(diff-hl-stage-current-hunk :which-key "Stage Hunk (diffhl)")
    "r" '(diff-hl-revert-hunk :which-key "Revert Hunk (diffhl)")
    "]" '(diff-hl-next-hunk :which-key "Next Hunk (diffhl)")
    "[" '(diff-hl-previous-hunk :which-key "Previous Hunk (diffhl)")
    )

  ;; Define the "G V" (VC) sub-submap under the C-c G prefix
  (general-def
    :prefix "C-c G V" ; Prefix for VC commands
    "d" '(vc-dir :which-key "VC Directory")
    "b" '(vc-annotate :which-key "VC Annotate")
    "=" '(vc-diff :which-key "VC Diff Buffer")
    "D" '(vc-root-diff :which-key "VC CWD Diff")
    "v" '(vc-next-action :which-key "VC Next Action"))

  ;; Define the "t" (Toggle) submap under the C-c leader
  (general-def
    :prefix "C-c t" ; Prefix for toggle commands
    "t" '(visual-line-mode :which-key "Visual Line")
    "l" '(display-line-numbers-mode :which-key "Line Numbers"))

  ;; Define the "O" (Org) submap under the C-c leader
  (general-def
    :prefix "C-c O" ; Prefix for Org commands
    "i" '(org-toggle-inline-images :which-key "Toggle Inline Images"))

  ;; Define the "O t" (TODO States) sub-submap under the C-c O prefix
  (general-def
    :prefix "C-c O t" ; Prefix for Org TODO commands
    "t" 'org-todo
    "d" '((lambda () (interactive) (org-todo "DOING")) :which-key "DOING")
    "h" '((lambda () (interactive) (org-todo "HOLD")) :which-key "HOLD")
    "D" '((lambda () (interactive) (org-todo "DONE")) :which-key "DONE")
    "c" '((lambda () (interactive) (org-todo "CANCELLED")) :which-key "CANCELLED")
    "m" '((lambda () (interactive) (org-todo "MAYBE")) :which-key "MAYBE")
    )

  ;; Define the "O a" (Org Agenda) sub-submap under the C-c O prefix
  (general-def
    :prefix "C-c O a" ; Prefix for Org Agenda commands
    "c" '(org-capture :which-key "Capture")
    "a" '(org-agenda :which-key "Agenda"))

  ;; Define the "O d" (Denote) sub-submap under the C-c O prefix
  (general-def
    :prefix "C-c O d" ; Prefix for Denote commands
    "n" '(denote :which-key "Denote")
    "r" '(denote-rename-file :which-key "Denote Rename")
    "l" '(denote-link :which-key "Denote Link")
    "b" '(denote-backlinks :which-key "Denote Backlinks")
    "o" '(denote-open-or-create :which-key "Denote Open/Create")
    "d" '(denote-dired :which-key "Denote Dired")
    "g" '(denote-grep :which-key "Denote Grep"))

  ;; Define the "l" (LSP) submap under the C-c leader
  (general-def
    :prefix "C-c L" ; Prefix for LSP commands
    "a" '(eglot-code-actions :which-key "Code Actions")
    "f" '(eglot-format-buffer :which-key "Format Buffer")
    "l" '(eglot-code-lens-action :which-key "Code-Lens Action")
    "n" '(eglot-rename :which-key "LSP Rename")
    "k" '(eldoc :which-key "LSP Documentation")
    "I" '(eglot-events-buffer :which-key "LSP Info")
    "d" '(xref-find-definitions :which-key "LSP Definition")
    "c" '(eglot-find-declaration :which-key "LSP Declaration")
    "i" '(eglot-find-implementation :which-key "LSP Implementation")
    "t" '(eglot-find-typeDefinition :which-key "LSP Type Definition")
    "r" '(xref-find-references :which-key "LSP References"))

  ;; Define the "L D" (Document) sub-submap under the C-c l 
  (general-def
    :prefix "C-c L D" ; Prefix for LSP Document commands
    "s" '(consult-imenu :which-key "Document Symbols")
    "d" '(consult-flymake :which-key "Document Diagnostics"))

  ;; Define the "l W" (Workspace) sub-submap under the C-c l prefix
  (general-def
    :prefix "C-c L W" ; Prefix for LSP Workspace commands
    ;; "a" 'projectile-add-known-project
    ;; "r" 'projectile-remove-known-project
    "s" '(consult-lsp-file-symbols :which-key "Workspace Symbols")
    "d" '((lambda () (interactive) (consult-flymake t)) :which-key "Workspace Diagnostics"))

  ;; Define global windmove bindings
  (general-def
    "M-<down>" '(windmove-down :which-key "Window Move Down")
    "M-<up>" '(windmove-up :which-key "Window Move Up")
    "M-<left>" '(windmove-left :which-key "Window Move Left")
    "M-<right>" '(windmove-right :which-key "Window Move Right")
    "M-j" '(windmove-down :which-key "Window Move Down")
    "M-k" '(windmove-up :which-key "Window Move Up")
    "M-h" '(windmove-left :which-key "Window Move Left")
    "M-l" '(windmove-right :which-key "Window Move Right")
    "C-<next>" '(scroll-up-line :which-key "Scroll Up Line")
    "C-<prior>" '(scroll-down-line :which-key "Scroll Down Line")
    "C-j" '(scroll-up-line :which-key "Scroll Up Line")
    "C-k" '(scroll-down-line :which-key "Scroll Down Line")
    "C-S-K" '(scroll-down-command :which-key "Page Up")
    "C-S-J" '(scroll-up-command :which-key "Page Down")
    "M-S-<right>" '(enlarge-window-horizontally :which-key "Window Width Increase")
    "M-S-<left>" '(shrink-window-horizontally :which-key "Window Width Decrease")
    "M-S-<up>" '(enlarge-window :which-key "Window Height Decrease")
    "M-S-<down>" '(shrink-window :which-key "Window Height Increase")
    "M-L" '(enlarge-window-horizontally :which-key "Window Width Increase")
    "M-H" '(shrink-window-horizontally :which-key "Window Width Decrease")
    "M-J" '(enlarge-window :which-key "Window Height Decrease")
    "M-K" '(shrink-window :which-key "Window Height Increase")
    )

  (general-def
    :prefix "C-c"
    "S ("  '(lambda (beg end) (interactive "r") (my-surround-region-pair beg end "(" ")"))
    "S {"  '(lambda (beg end) (interactive "r") (my-surround-region-pair beg end "{" "}"))
    "S \"" '(lambda (beg end) (interactive "r") (my-surround-region-pair beg end "\"" "\""))
    "S ["  '(lambda (beg end) (interactive "r") (my-surround-region-pair beg end "[" "]"))
    "S <"  '(lambda (beg end) (interactive "r") (my-surround-region-pair beg end "<" ">"))
    "S '"  '(lambda (beg end) (interactive "r") (my-surround-region-pair beg end "`" "'")))

  (general-def
    :prefix "C-x"
    "_" '(split-window-below :which-key "Split Below")
    "|" '(split-window-right :which-key "Split Right")
    ))
;;-----------------------------------------------------------------------------
;; Convenience functions
;;-----------------------------------------------------------------------------
(defun my-surround-region-pair (beg end left-char right-char)
  "Surround the active region (BEG to END) with LEFT-CHAR and RIGHT-CHAR.
This is a non-interactive helper function."
  (let ((original-beg beg))
    (goto-char end)
    (insert right-char)
    (goto-char beg)
    (insert left-char)
    (deactivate-mark)
    (goto-char original-beg))
  )

;;; init.el ends here

