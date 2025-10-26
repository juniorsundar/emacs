;;; init.el --- My Emacs Config -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Author: Junior Sundar
;; Version: 0.1.0
;; Package-Requires: ((Emacs "30.0"))
;;
;;; Code:

(setq gc-cons-threshold #x40000000)
(setq read-process-output-max (* 1024 1024 4))

;;-----------------------------------------------------------------------------
;; Set up use-package and add possibility for custom configs
;;-----------------------------------------------------------------------------
(require 'use-package-ensure)
(setq use-package-always-ensure t)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
						 ("org" . "https://orgmode.org/elpa/")
						 ("elpa" . "https://elpa.gnu.org/packages/")
						 ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(defvar my-config-dir (expand-file-name "lisp" user-emacs-directory)
  "Directory containing my configuration files.")
(defun load-config-file (file)
  (load (expand-file-name file my-config-dir)))

;;-----------------------------------------------------------------------------
;; Add to search paths
;;----------------------------------------------------------------------------
(dolist (path '("/usr/local/go/bin"
                "/usr/local/bin"
                "~/.local/bin"
                "~/go/bin"
                "/usr/bin"
                "~/anaconda3/bin"
                "~/.nvm/versions/node/v20.15.0/bin/"))
  (add-to-list 'exec-path (expand-file-name path)))

;;-----------------------------------------------------------------------------
;; Default Emacs Configurations
;;-----------------------------------------------------------------------------
(use-package emacs
  :ensure nil
  :custom
  (frame-resize-pixelwise t)
  (auto-save-default nil)                         ;; Disable automatic saving of buffers.
  (create-lockfiles nil)                          ;; Prevent the creation of lock files when editing.
  (delete-by-moving-to-trash t)                   ;; Move deleted files to the trash instead of permanently deleting them.
  (delete-selection-mode t)                       ;; Enable replacing selected text with typed text.
  (display-line-numbers-type 'relative)           ;; Use relative line numbering.
  ;; (global-display-line-numbers-mode t)            ;; Enable line numbers globally.
  (global-auto-revert-non-file-buffers t)         ;; Automatically refresh non-file buffers.
  (global-auto-revert-mode t)                     ;; Enable global auto-revert mode for files.
  (history-length 25)                             ;; Set the length of the command history.
  (inhibit-startup-screen t)                      ;; Disable the startup screen.
  (initial-scratch-message "")                    ;; Clear the initial message in the *scratch* buffer.
  (ispell-program-name "aspell")
  (ispell-dictionary "en_US")                     ;; Set the default dictionary for spell checking.
  (make-backup-files nil)                         ;; Disable creation of backup files.
  (pixel-scroll-precision-mode t)                 ;; Enable precise pixel scrolling.
  (pixel-scroll-precision-use-momentum nil)       ;; Disable momentum scrolling.
  (ring-bell-function 'ignore)                    ;; Disable the audible bell.
  (split-width-threshold 300)                     ;; Prevent automatic window splitting.
  (switch-to-buffer-obey-display-actions t)       ;; Make buffer switching respect display actions.
  (tab-always-indent 'complete)                   ;; Make TAB key complete text instead of just indenting.
  (tab-width 4)                                   ;; Set the tab width to 4 spaces.
  (truncate-lines t)                              ;; Enable line truncation to avoid wrapping long lines.
  (use-dialog-box nil)                            ;; Disable dialog boxes.
  (use-short-answers t)                           ;; Use short answers (y/n).
  (warning-minimum-level :emergency)              ;; Only show emergency warnings.
  (mouse-wheel-progressive-speed nil)             ;; Disable progressive scrolling.
  (scroll-conservatively 10)                      ;; Enable smooth scrolling.
  (enable-recursive-minibuffers t)                ;; Allow recursive minibuffers.
  (electric-pair-mode t)                          ;; Enable automatic parenthesis pairing.
  (blink-cursor-mode nil)                         ;; Disable cursor blinking.
  (xterm-mouse-mode 1)                            ;; Enable mouse support in terminal.
  (recentf-mode t)                                ;; Enable recent file tracking.
  (savehist-mode t)                               ;; Enable command history saving.
  (save-place-mode t)                             ;; Enable saving of the last visited place in files.
  (winner-mode t)                                 ;; Enable window configuration undo.
  (file-name-shadow-mode t)                       ;; Enable shadowing of filenames for clarity.
  (treesit-font-lock-level 4)                     ;; Advanced font locking with Treesit.

  :init
  (tool-bar-mode -1)                              ;; Disable the toolbar.
  (menu-bar-mode -1)                              ;; Disable the menu bar.
  (scroll-bar-mode -1)                            ;; Disable the scroll bar.
  (global-hl-line-mode 1)                         ;; Highlight the current line.
  (indent-tabs-mode -1)                           ;; Disable the use of tabs for indentation (use spaces).
  (modify-coding-system-alist 'file "" 'utf-8)    ;; Set the default file encoding to UTF-8.

  ;; Minibuffer Improvements
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt)) ;; Read-only minibuffer prompt.
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Welcome message
  (add-hook 'after-init-hook
            (lambda ()
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

(savehist-mode)

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

(use-package ibuffer-projectile
  :ensure t
  :hook (ibuffer . ibuffer-projectile-set-filter-groups)
  :config
  (setq ibuffer-projectile-prefix
        (if (and (display-graphic-p) (require 'nerd-icons nil t))
            (concat (nerd-icons-octicon "nf-oct-file_directory"
										:face 'ibuffer-filter-group-name-face
										:v-adjust -0.05)
                    " ")
          "Project: ")))

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
		(setq insert-directory-program gls)))))

(use-package diredfl
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
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package diminish)

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
              ("C-M-<up>" . evil-window-increase-height)
              ("C-M-k" . evil-window-increase-height)
              ("C-M-<down>" . evil-window-decrease-height)
              ("C-M-j" . evil-window-decrease-height)
              ("C-M-<right>" . evil-window-increase-width)
              ("C-M-l" . evil-window-increase-width)
              ("C-M-<left>" . evil-window-decrease-width)
              ("C-M-h" . evil-window-decrease-width)
              )
  (:map evil-visual-state-map
		("M-h"      . evil-shift-left)
		("M-l"      . evil-shift-right)
		("M-<left>" . evil-shift-left)
		("M-<right>". evil-shift-right)
		)
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

(use-package avy
  :ensure t)

(use-package evil-commentary
  :after evil
  :config (evil-commentary-mode))

(use-package evil-avy
  :after (evil avy)
  :config
  (evil-define-key '(normal visual) 'global "s" #'avy-goto-word-1))

;;-----------------------------------------------------------------------------
;; Theme
;;-----------------------------------------------------------------------------
;; (load-config-file "catppuccin-theme/catppuccin-theme.el")
;; (load-theme 'catppuccin :no-confirm) ;; We need to add t to trust this package
;; (setq catppuccin-flavor 'cyberdream)
;; (setq catppuccin-italic-comments 't)
;; (catppuccin-reload)

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

(use-package solaire-mode
  :ensure t
  :config
  (solaire-global-mode +1)
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

(defun my/set-frame-fonts (frame)
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

(add-hook 'after-make-frame-functions #'my/set-frame-fonts)
(when (and (not (daemonp)) (display-graphic-p))
  (my/set-frame-fonts (selected-frame)))

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
  (doom-modeline-bar-width 5)
  (doom-modeline-buffer-name t)
  (doom-modeline-vcs-max-length 25)
  (doom-modeline-persp-name t)
  (doom-modeline-persp-icon t)
  (doom-modeline-buffer-file-name-style 'buffer-name)
  (doom-modeline-project-detection 'project)
  (doom-modeline-icon t)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-enable-word-count t))
;;-----------------------------------------------------------------------------
;; Projectile
;;-----------------------------------------------------------------------------
(use-package projectile
  :init
  (projectile-mode)
  :custom
  (projectile-run-use-comint-mode t)
  (projectile-switch-project-action #'projectile-dired)
  (projectile-project-search-path '("~/Documents/Projects/"
									;; "~/Documents/work/"
									)
								  )
  )

;;-----------------------------------------------------------------------------
;; LSP and Language Modes
;;-----------------------------------------------------------------------------
(use-package direnv
  :config
  (direnv-mode))

;; (use-package lsp-mode
;;   :init
;;   ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
;;   ;; (setq lsp-keymap-prefix "SPC L")
;;   :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
;;          (rust-ts-mode . lsp)
;;          (lsp-mode . lsp-enable-which-key-integration))
;;   :commands lsp)
;; (use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

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
(defun my/close-eldoc-buffer-if-left ()
  "Close *eldoc* buffer if point is no longer in it."
  (unless (or (null (get-buffer "*eldoc*"))
              (eq (current-buffer) (get-buffer "*eldoc*")))
    (let ((win (get-buffer-window "*eldoc*")))
      (when win
        (quit-window t win)))))
(add-hook 'post-command-hook #'my/close-eldoc-buffer-if-left)

(use-package yasnippet-snippets
  :hook (prog-mode . yas-minor-mode))

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (setq treesit-auto-add-to-auto-mode-alist 'all)
  (setq global-treesit-auto-mode t))

(use-package treesit-fold
  :ensure t
  :hook (prog-mode . global-treesit-fold-mode)
  :config
  (evil-define-key '(normal visual) 'global
    "za" #'treesit-fold-toggle
    "zc" #'treesit-fold-close
    "zo" #'treesit-fold-open
    "zC" #'treesit-fold-close-all
    "zO" #'treesit-fold-open-all
    "zR" #'treesit-fold-open-all
    "zM" #'treesit-fold-close-all
    "zr" #'treesit-fold-open-recursively
    "zm" #'treesit-fold-close)
  )

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
(use-package magit
  :ensure t
  :commands (magit-status magit-blame-addition)
  )

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
  )

(use-package embark
  :ensure t
  :defer t
  :bind
  (("C-q" . embark-act)
   ("C-d" . embark-dwim)
   ("C-h B" . embark-bindings))
  )

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package wgrep
  :ensure t)

;;-----------------------------------------------------------------------------
;; Key Binders
;;-----------------------------------------------------------------------------
(use-package general
  :config
  (general-evil-setup)
  (general-create-definer start/leader-keys
	:states '(normal insert visual motion emacs)
	:keymaps 'override
	:prefix "SPC"
	:global-prefix "C-SPC")
  
  (start/leader-keys
	"." '(find-file :wk "Find file")
	"u" '(vundo :wk "UndoTree")
	"P" '(projectile-command-map :wk "Projectile command map"))
  
  (start/leader-keys
	"z" '(:ignore t :wk "Hide-Show")
	"z a" '(hs-toggle-hiding :wk "Toggle fold")
	"z c" '(hs-hide-block :wk "Fold block")
	"z o" '(hs-show-block :wk "Reveal block")
	"z R" '(hs-show-all :wk "Reveal all")
	"z M" '(hs-hide-all :wk "Fold all")
	)

  (start/leader-keys
	"F" '(:ignore t :wk "Find")
	"F c" '((lambda () (interactive) (find-file "~/.config/emacs/init.el")) :wk "Edit emacs config")
	"F r" '(consult-recent-file :wk "Recent files")
	"F f" '(consult-fd :wk "Fd search for files")
	"F t" '(consult-ripgrep :wk "Ripgrep search in files")
	"F l" '(consult-line :wk "Find line")
	)

  (start/leader-keys
	"b" '(:ignore t :wk "Buffer Bookmarks")
	"b b" '(consult-buffer :wk "Switch buffer")
	"b k" '(kill-this-buffer :wk "Kill this buffer")
	"b i" '(ibuffer :wk "Ibuffer")
	"b n" '(next-buffer :wk "Next buffer")
	"b p" '(previous-buffer :wk "Previous buffer")
	"b r" '(revert-buffer :wk "Reload buffer")
	"b j" '(consult-bookmark :wk "Bookmark jump"))
  
  (start/leader-keys
	"G" '(:ignore t :wk "Git")
	"G g" '(magit-status :wk "Magit status")
	"G l" '(magit-log-current :wk "Current log")
	"G d" '(magit-diff-buffer-file :wk "Diff current file")
	"G D" '(diff-hl-show-hunk :wk "Diff hunk")
	"G V" '(:ignore t :wk "VC")
	"G V d" '(vc-dir :wk "VC directory")
	"G V b" '(vc-annotate :wk "VC annotate buffer")
	"G V =" '(vc-diff :wk "VC diff current file")
	"G V D" '(vc-root-diff :wk "VC diff entire repository")
	"G V v" '(vc-next-action :wk "Next VC action")
	)
  
  (start/leader-keys
	"H" '(:ignore t :wk "Describe")
	"H m" '(describe-mode :wk "Describe current mode")
	"H f" '(describe-function :wk "Describe function")
	"H v" '(describe-variable :wk "Describe variable")
	"H k" '(describe-key :wk "Describe key")
	)

  (start/leader-keys
	"t" '(:ignore t :wk "Toggle")
	"t t" '(visual-line-mode :wk "Toggle truncated lines (wrap)")
	"t l" '(display-line-numbers-mode :wk "Toggle line numbers"))
  
  (start/leader-keys
	"O" '(:ignore t :which-key "Org")
	"O i" '(org-toggle-inline-images :wk "Toggle Inline Images")

	"O t" '(:ignore t :which-key "TODO States")
	"O t t" '(org-todo :which-key "Set TODO")
	"O t d" '((lambda () (interactive) (org-todo "DOING")) :which-key "Set DOING")
	"O t h" '((lambda () (interactive) (org-todo "HOLD")) :which-key "Set HOLD")
	"O t D" '((lambda () (interactive) (org-todo "DONE")) :which-key "Set DONE")
	"O t c" '((lambda () (interactive) (org-todo "CANCELLED")) :which-key "Set CANCELLED")
	"O t m" '((lambda () (interactive) (org-todo "MAYBE")) :which-key "Set MAYBE"))
  
  (start/leader-keys
	"O a" '(:ignore t :wk "Org Agenda")
	"O a c" '(org-capture :wk "Capture")
	"O a a" '(org-agenda :wk "Agenda")

	"O r" '(:ignore t :wk "Org Roam")
	"O r l" '(org-roam-buffer-toggle :wk "Toggle Buffer")
	"O r f" '(org-roam-node-find :wk "Find Node")
	"O r i" '(org-roam-node-insert :wk "Insert Node")
	"O r c" '(org-roam-capture :wk "Capture")
	"O r g" '(org-roam-graph :wk "Graph"))
  
  (start/leader-keys
	"O d" '(:ignore t :wk "Org Roam Dailies")
	"O d t" '(org-roam-dailies-capture-today :wk "Capture Today")
	"O d y" '(org-roam-dailies-capture-yesterday :wk "Capture Yesterday")
	"O d d" '(org-roam-dailies-goto-date :wk "Go-to Date")
	"O d T" '(org-roam-dailies-goto-today :wk "Go-to Today")
	"O d Y" '(org-roam-dailies-goto-yesterday :wk "Go-to Yesterday"))

  (start/leader-keys
	"-" '((lambda () (interactive) (dired default-directory)) :wk "Open"))

  (start/leader-keys
	"L"  '(:ignore t :wk "LSP")
	;; "L a" '(lsp-execute-code-action :wk "Code Action")
	;; "L f" '(lsp-format-buffer :wk "Format Buffer")
	;; "L l" '(lsp-lens-mode :wk "CodeLens Mode")
	;; "L n" '(lsp-rename :wk "Rename")
	;; "L k" '(lsp-describe-thing-at-point :wk "Hover Documentation")
	;; "L I" '(lsp-describe-session :wk "LSP Info")
	;; "L d" '(lsp-find-definition :wk "Definition")
	;; "L c" '(lsp-find-declaration :wk "Declaration")
	;; "L i" '(lsp-find-implementation :wk "Implementation")
	;; "L t" '(lsp-find-typeDefinition :wk "Type Definition")
	;; "L r" '(lsp-find-references :wk "References")
	"L a" '(eglot-code-actions :wk "Code Action")
	"L f" '(eglot-format-buffer :wk "Format Buffer")
	"L l" '(eglot-code-lens-action :wk "CodeLens Action")
	"L n" '(eglot-rename :wk "Rename")
	"L k" '(eldoc :wk "Hover Documentation")
	"L I" '(eglot-events-buffer :wk "LSP Info")
	"L d" '(xref-find-definitions :wk "Definition")
	"L c" '(eglot-find-declaration :wk "Declaration")
	"L i" '(eglot-find-implementation :wk "Implementation")
	"L t" '(eglot-find-typeDefinition :wk "Type Definition")
	"L r" '(xref-find-references :wk "References")

    ;; Document actions
    "L D" '(:ignore t :wk "Document")
    "L D s" '(consult-imenu :wk "Symbols")
    "L D d" '(consult-flymake :wk "Diagnostics")

    ;; Workspace actions
    "L W" '(:ignore t :wk "Workspace")
    "L W a" '(projectile-add-known-project :wk "Add Folder")
    "L W r" '(projectile-remove-known-project :wk "Remove Folder")
    "L W l" '(consult-project-buffer :wk "List Folders (Buffers)")
    "L W s" '(consult-lsp-file-symbols :wk "Symbols")
    "L W d" '((lambda () (interactive) (consult-flymake t)) :wk "Diagnostics (Project)"))
  )

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

(defun my/org-agenda-files-recursive (directory)
  "Recursively find all .org files in DIRECTORY."
  (directory-files-recursively directory "\\.org$"))
(defun my/update-org-agenda-files (&rest _)
  "Update `org-agenda-files` to include all .org files in the directory."
  (setq org-agenda-files (my/org-agenda-files-recursive "~/Dropbox/org/")))

(advice-add 'org-agenda :before #'my/update-org-agenda-files)

(defun my/kill-agenda-file-buffers ()
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

(custom-set-faces
 '(org-code ((t (:background "#1e2124" :foreground "#ffffff" :family "IosevkaTerm Nerd Font")))))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/Dropbox/org/pages"))
  (org-roam-dailies-directory "./journals/")
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
	  :target (file+head "pages/${slug}.org" "#+title: ${title}\n")
	  :unnarrowed t))
   org-roam-dailies-capture-templates
   '(("d" "default" entry "* %?"
	  :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n#+FILETAGS: journal"))))

  :config
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  (require 'org-roam-protocol))

(use-package toc-org
  :commands toc-org-enable
  :hook (org-mode . toc-org-mode))

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-headline-bullets-list '("󰼏 " "󰼐 " "󰼑 " "󰼒 " "󰼓 " "󰼔 "))
  )

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

;;; init.el ends here
