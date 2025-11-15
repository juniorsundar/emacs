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

(unless (package-installed-p 'quelpa)
  (with-temp-buffer
	(url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
	(eval-buffer)
	(quelpa-self-upgrade)))
(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)

;;-----------------------------------------------------------------------------
;; Add to search paths
;;----------------------------------------------------------------------------
(dolist (path '("/usr/local/go/bin"
                "/usr/local/bin"
                "~/.local/bin"
                "/usr/bin"))
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
  (global-auto-revert-non-file-buffers t)         ;; Automatically refresh non-file buffers.
  (global-auto-revert-mode t)                     ;; Enable global auto-revert mode for files.
  (history-length 25)                             ;; Set the length of the command history.
  (inhibit-startup-screen t)                      ;; Disable the startup screen.
  (initial-scratch-message "")                    ;; Clear the initial message in the *scratch* buffer.
  (ispell-program-name "aspell")
  (ispell-dictionary "en_US")                     ;; Set the default dictionary for spell checking.
  (make-backup-files nil)                         ;; Disable creation of backup files.
  (line-number-mode nil)
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

(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "-") #'dired-up-directory)))

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
  (doom-modeline-bar-width 5)
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

(use-package yasnippet-snippets
  :hook (prog-mode . yas-minor-mode))

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
(use-package magit
  :ensure t
  :commands (magit-status magit-blame-addition)
  )
(setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1
      magit-bury-buffer-function #'magit-mode-quit-window)

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
  :defer t
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
(use-package meow
  :ensure t)
(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<left>"  . meow-left)
   '("<right>" . meow-right)
   '("<up>"    . meow-prev)
   '("<down>"  . meow-next)
   '("<S-left>"  . meow-left-expand)
   '("<S-right>" . meow-right-expand)
   '("<S-up>"    . meow-prev-expand)
   '("<S-down>"  . meow-next-expand)
   '("S (" . (lambda (beg end) (interactive "r") (my-surround-region-pair beg end "(" ")")))
   '("S {" . (lambda (beg end) (interactive "r") (my-surround-region-pair beg end "{" "}")))
   '("S \"" . (lambda (beg end) (interactive "r") (my-surround-region-pair beg end "\"" "\"")))
   '("S [" . (lambda (beg end) (interactive "r") (my-surround-region-pair beg end "[" "]")))
   '("S <" . (lambda (beg end) (interactive "r") (my-surround-region-pair beg end "<" ">")))
   '("S '" . (lambda (beg end) (interactive "r") (my-surround-region-pair beg end "`" "'")))
   '("<escape>" . ignore)))
(require 'meow)
(meow-setup)
(meow-global-mode 1)

;;-----------------------------------------------------------------------------
;; General Keybindings
;;-----------------------------------------------------------------------------
(use-package general
  :ensure t)
(require 'general)
(general-def
  :prefix "C-c" ; Use C-c as the leader key
  :non-normal-prefix "C-c" ; Also apply to non-normal states if using evil
  ;; Top-level bindings under C-c
  "." '(find-file :which-key "Find")
  "P" '(projectile-command-map :which-key "Projectile")
  "-" '((lambda () (interactive) (dired default-directory)) :which-key "Dired File")
  ;; Avy bindings under C-c (as top-level under the prefix)
  "C-s" 'avy-goto-char
  "C-j" 'avy-goto-line
  "C-k" 'avy-goto-line
  "C-<up>" 'avy-goto-line
  "C-<down>" 'avy-goto-line
  )

;; Define the "Z" (Hide-Show) submap under the C-c leader
(general-def
  :prefix "C-c Z" ; Prefix for hide-show commands
  "a" 'hs-toggle-hiding
  "c" 'hs-hide-block
  "o" 'hs-show-block
  "R" 'hs-show-all
  "M" 'hs-hide-all)

;; Define the "F" (Find) submap under the C-c leader
(general-def
  :prefix "C-c F" ; Prefix for find commands
  "c" (lambda () (interactive) (find-file "~/.config/emacs/init.el"))
  "r" 'consult-recent-file
  "f" 'consult-fd
  "t" 'consult-ripgrep
  "l" 'consult-line)

;; Define the "B" (Buffer Bookmarks) submap under the C-c leader
(general-def
  :prefix "C-c B" ; Prefix for buffer/bookmark commands
  "b" 'consult-buffer
  "k" 'kill-this-buffer
  "i" 'ibuffer
  "n" 'next-buffer
  "p" 'previous-buffer
  "r" 'revert-buffer
  "j" 'consult-bookmark)

;; Define the "G" (Git) submap under the C-c leader
(general-def
  :prefix "C-c G" ; Prefix for git commands
  "g" 'magit-status
  "l" 'magit-log-current
  "d" 'magit-diff-buffer-file
  "D" 'diff-hl-show-hunk)

;; Define the "G V" (VC) sub-submap under the C-c G prefix
(general-def
  :prefix "C-c G V" ; Prefix for VC commands
  "d" 'vc-dir
  "b" 'vc-annotate
  "=" 'vc-diff
  "D" 'vc-root-diff
  "v" 'vc-next-action)

;; Define the "H" (Describe) submap under the C-c leader
(general-def
  :prefix "C-c H" ; Prefix for describe commands
  "m" 'describe-mode
  "f" 'describe-function
  "v" 'describe-variable
  "k" 'describe-key)

;; Define the "t" (Toggle) submap under the C-c leader
(general-def
  :prefix "C-c t" ; Prefix for toggle commands
  "t" 'visual-line-mode
  "l" 'display-line-numbers-mode)

;; Define the "O" (Org) submap under the C-c leader
(general-def
  :prefix "C-c O" ; Prefix for Org commands
  "i" 'org-toggle-inline-images)

;; Define the "O t" (TODO States) sub-submap under the C-c O prefix
(general-def
  :prefix "C-c O t" ; Prefix for Org TODO commands
  "t" 'org-todo
  "d" (lambda () (interactive) (org-todo "DOING"))
  "h" (lambda () (interactive) (org-todo "HOLD"))
  "D" (lambda () (interactive) (org-todo "DONE"))
  "c" (lambda () (interactive) (org-todo "CANCELLED"))
  "m" (lambda () (interactive) (org-todo "MAYBE")))

;; Define the "O a" (Org Agenda) sub-submap under the C-c O prefix
(general-def
  :prefix "C-c O a" ; Prefix for Org Agenda commands
  "c" 'org-capture
  "a" 'org-agenda)

;; Define the "O d" (Denote) sub-submap under the C-c O prefix
(general-def
  :prefix "C-c O d" ; Prefix for Denote commands
  "n" 'denote
  "r" 'denote-rename-file
  "l" 'denote-link
  "b" 'denote-backlinks
  "o" 'denote-open-or-create
  "d" 'denote-dired
  "g" 'denote-grep)

;; Define the "l" (LSP) submap under the C-c leader
(general-def
  :prefix "C-c l" ; Prefix for LSP commands
  "a" 'eglot-code-actions
  "f" 'eglot-format-buffer
  "l" 'eglot-code-lens-action
  "n" 'eglot-rename
  "k" 'eldoc
  "I" 'eglot-events-buffer
  "d" 'xref-find-definitions
  "c" 'eglot-find-declaration
  "i" 'eglot-find-implementation
  "t" 'eglot-find-typeDefinition
  "r" 'xref-find-references)

;; Define the "l D" (Document) sub-submap under the C-c l prefix
(general-def
  :prefix "C-c l D" ; Prefix for LSP Document commands
  "s" 'consult-imenu
  "d" 'consult-flymake)

;; Define the "l W" (Workspace) sub-submap under the C-c l prefix
(general-def
  :prefix "C-c l W" ; Prefix for LSP Workspace commands
  "a" 'projectile-add-known-project
  "r" 'projectile-remove-known-project
  "l" 'consult-project-buffer
  "s" 'consult-lsp-file-symbols
  "d" (lambda () (interactive) (consult-flymake t)))

;; Define global windmove bindings
(general-def
  "M-<down>" 'windmove-down
  "M-<up>" 'windmove-up
  "M-<left>" 'windmove-left
  "M-<right>" 'windmove-right
  "M-j" 'windmove-down
  "M-k" 'windmove-up
  "M-h" 'windmove-left
  "M-l" 'windmove-right
  "C-<next>" 'scroll-up-line
  "C-<prior>" 'scroll-down-line
  )

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

(defun my-surround-region-prompt (beg end)
  "Surround the active region (BEG to END) with prompted chars."
  (interactive "r") ; <-- This is the main fix. "r" requires a region.
  (let* ((chars (split-string (read-string "Surround with (e.g., (), [], {}, '', \"\"): ") "" t))
         (left-char nil)
         (right-char nil))
    
    (cond
     ((= (length chars) 1) ; Single character
      (let ((char (car chars)))
        (cond
         ((string= char "'") (setq left-char "`" right-char "'"))
         ((string= char "\"") (setq left-char "\"" right-char "\""))
         ((string= char "`") (setq left-char "`" right-char "'"))
         ((string= char "<") (setq left-char "<" right-char ">"))
         (t (user-error "Unknown single character alias: %s" char)))))
     ((= (length chars) 2) ; Two characters
      (setq left-char (car chars) right-char (cadr chars)))
     (t (user-error "Please enter one or two characters (e.g., (), [], ', \")"))))
  
  (when (and left-char right-char)
    (my-surround-region-pair beg end left-char right-char)))

;;; init.el ends here

