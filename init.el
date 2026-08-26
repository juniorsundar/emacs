;;; init.el --- My Emacs Config -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Author: Junior Sundar
;; Version: 0.1.0
;; Package-Requires: ((Emacs "30.0"))
;;
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
  (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

  ;; Enable global-auto-revert-mode
  (global-auto-revert-mode 1)

  :hook
  ;; (prog-mode . display-line-numbers-mode)
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
                " " (name 18 18 :left :elide)
                " " (size 9 -1 :right)
                " " (mode 16 16 :left :elide)
                " " filename-and-process)
          (mark " " (name 16 -1) " " filename)))

  (define-ibuffer-column size (:name "Size" :inline t)
    (file-size-human-readable (buffer-size)))

  ;; (evil-define-key 'normal ibuffer-mode-map "q" #'kill-current-buffer)
  )

(use-package ibuffer-vc
  :ensure t
  :config
  (add-hook 'ibuffer-hook
	    (lambda ()
	      (ibuffer-vc-set-filter-groups-by-vc-root)
	      (unless (eq ibuffer-sorting-mode 'alphabetic)
		(ibuffer-do-sort-by-alphabetic))))
  )

(use-package dired
  :ensure nil                                                ;; This is built-in, no need to fetch it.
  :custom
  (dired-listing-switches "-lah -v --group-directories-first")  ;; Display files in a human-readable format and group directories first.
  (dired-dwim-target t)                                      ;; Enable "do what I mean" for target directories.
  (dired-guess-shell-alist-user
   '(("\\.\\(png\\|jpe?g\\|tiff\\)" "feh" "xdg-open" "open") ;; Open image files with `feh' or the default viewer.
     ("\\.\\(mp[34]\\|m4a\\|ogg\\|flac\\|webm\\|mkv\\)" "mpv" "xdg-open" "open") ;; Open audio and video files with `mpv`.
     (".*" "open" "xdg-open")))                              ;; Default opening command for other files.
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-auto-revert-buffer #'dired-buffer-stale-p)
  (dired-recursive-copies  'always)
  (dired-recursive-deletes 'top)
  (dired-create-destination-dirs 'ask)
  :config
  (when (eq system-type 'darwin)
    (let ((gls (executable-find "gls")))
      (when gls
	(setq insert-directory-program gls))))
  (add-hook 'dired-mode-hook
            (lambda ()
              (define-key dired-mode-map (kbd "-") #'dired-up-directory)))
  ;; Image-dired paths - use user's emacs directory for cache
  (let ((cache-dir (locate-user-emacs-file "cache/image-dired/")))
    (setq image-dired-dir cache-dir)
    (setq image-dired-db-file (concat cache-dir "db.el"))
    (setq image-dired-gallery-dir (concat cache-dir "gallery/"))
    (setq image-dired-temp-image-file (concat cache-dir "temp-image"))
    (setq image-dired-temp-rotate-image-file (concat cache-dir "temp-rotate-image")))
  (setq image-dired-thumb-size 150)
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

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

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
;; Centralized font configuration - edit these variables to change fonts globally
(defcustom my/font-fixed-family "Lilex Nerd Font"
  "Monospace font family for code and fixed-pitch text."
  :type 'string
  :group 'fonts)

(defcustom my/font-variable-family "IBM Plex Sans"
  "Proportional font family for headings and variable-pitch text."
  :type 'string
  :group 'fonts)

(defcustom my/font-emoji-family "Noto Color Emoji"
  "Emoji font family."
  :type 'string
  :group 'fonts)

(defcustom my/font-fixed-height 100
  "Height for fixed-pitch faces (monospace)."
  :type 'integer
  :group 'fonts)

(defcustom my/font-variable-height 120
  "Height for variable-pitch faces (proportional)."
  :type 'integer
  :group 'fonts)

(defun fixed-pitch-mode ()
  (interactive)
  (buffer-face-mode -1))
(defun variable-pitch-mode ()
  (interactive)
  (buffer-face-mode t))
(defun toggle-pitch ()
  "Switch between the `fixed-pitch' face and the `variable-pitch' face"
  (interactive)
  (buffer-face-toggle 'variable-pitch))
(buffer-face-mode)

(add-hook 'eww-mode-hook 'variable-pitch-mode)

(defun my/set-font-for-frame (frame)
  "Apply centralized font settings to FRAME."
  (when (display-graphic-p frame)
    (with-selected-frame frame
      ;; Set base faces
      (set-face-attribute 'default nil :family my/font-fixed-family :height my/font-fixed-height)
      (set-face-attribute 'variable-pitch nil :family my/font-variable-family :height my/font-variable-height)
      (copy-face 'default 'fixed-pitch)

      ;; Enable buffer-face-mode for variable-pitch support
      (buffer-face-mode)

      ;; Set up emoji font
      (when (member my/font-emoji-family (font-family-list))
        (set-fontset-font
         t 'symbol (font-spec :family my/font-emoji-family) nil 'prepend))

      (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
      )))

(add-hook 'after-make-frame-functions #'my/set-font-for-frame)
(when (and (not (daemonp)) (display-graphic-p))
  (my/set-font-for-frame (selected-frame)))

(add-hook 'ibuffer-mode-hook (lambda () (display-line-numbers-mode -1)))

(use-package nerd-icons
  :ensure t
  :if (display-graphic-p))

(use-package nerd-icons-ibuffer
  :ensure t
  :after nerd-icons
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package nerd-icons-dired
  :ensure t
  :after nerd-icons
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-corfu
  :ensure t
  :after (:all corfu)
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

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
  (doom-modeline-modal t)
  (doom-modeline-percent-position nil)
  (doom-modeline-buffer-file-name-style 'buffer-name)
  (doom-modeline-project-detection 'project)
  (doom-modeline-icon t)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-enable-word-count t)
  )

(use-package smerge-mode
  :ensure nil
  :defer t)

;;-----------------------------------------------------------------------------
;; Evil
;;-----------------------------------------------------------------------------
(use-package evil
  :ensure t
  :init
  (defvar evil-mode-buffers nil)
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-undo-system 'undo-redo)
  :config
  (evil-mode 1)
  ;; Match the existing windmove commands with Vim-style C-w arrow variants.
  (evil-define-key 'normal 'global (kbd "C-w <left>") #'windmove-left)
  (evil-define-key 'normal 'global (kbd "C-w <down>") #'windmove-down)
  (evil-define-key 'normal 'global (kbd "C-w <up>") #'windmove-up)
  (evil-define-key 'normal 'global (kbd "C-w <right>") #'windmove-right)
  ;; Close only the current emacsclient frame; leave the daemon running.
  (evil-define-key 'normal 'global (kbd "ZZ") #'delete-frame))

(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-snipe
  :ensure t
  :after evil
  :config
  ;; Keep Evil's native f/F/t/T motions; Snipe adds two-character s/S motions.
  (evil-snipe-mode 1))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

(use-package avy
  :ensure t
  :defer t)

(use-package wgrep
  :ensure t
  :defer t)

(use-package multiple-cursors
  :ensure t
  :defer t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))

;; -----------------------------------------------------------------------------
;; Completions
;;-----------------------------------------------------------------------------

(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.2)
  (corfu-separator ?\s)
  (corfu-quit-at-boundary 'separator)
  (corfu-preview-current nil)
  (corfu-popupinfo-delay 0.1)
  (completion-ignore-case t)
  :init
  (global-corfu-mode)
  :config
  (corfu-popupinfo-mode 1))

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
  :config
  (advice-add 'dabbrev-capf :override #'ignore))

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
   consult-source-bookmark consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file
   :preview-key '(:debounce 0.2 any))
  (setq consult-narrow-key "<")
  (setq consult-buffer-sources
	'(consult-source-project-buffer ; 1. Show project buffers first
	  consult-source-buffer         ; 2. Then show all other buffers
	  consult-source-recent-file
	  consult-source-bookmark))
  (advice-add #'consult--buffer-filter :override
              (lambda (buffer) (not (consult--project-buffer-p buffer))))
  (setq consult-fd-args
	`((if (executable-find "fdfind" 'remote) "fdfind" "fd")
	  "--color=never"
	  ;; https://github.com/sharkdp/fd/issues/839
	  "--hidden --exclude .git"
	  ,@(if (featurep :system 'windows) '("--path-separator=/"))))
  )

(use-package embark
  :ensure t
  :bind
  (("C-h B" . embark-bindings)))

(use-package embark-consult
  :ensure t
  :after embark)

(use-package ghostel
  :defer t
  :vc (:url "https://github.com/dakra/ghostel"
	    :lisp-dir "lisp"
	    :rev :newest)
  :init
  (with-eval-after-load 'project
    (add-to-list 'project-switch-commands '(ghostel-project "Ghostel") t))
  :config

  (defun my/ghostel-project-buffers (orig-fun project)
    (let* ((root (ignore-errors (file-truename (project-root project))))
           (by-dir (and root
                        (cl-remove-if-not
                         (lambda (b)
                           (when (buffer-live-p b)
                             (with-current-buffer b
                               (and default-directory
                                    (not (file-remote-p default-directory))
                                    (string-prefix-p
                                     root (file-truename default-directory))))))
                         (buffer-list))))
           (by-identity (and (featurep 'ghostel)
                             (ignore-errors (ghostel--project-buffers)))))
      (seq-union (funcall orig-fun project)
                 (seq-union by-dir by-identity))))
  (advice-add #'project-buffers :around #'my/ghostel-project-buffers))

;;-----------------------------------------------------------------------------
;; Flymake (built-in diagnostics)
;;-----------------------------------------------------------------------------
(use-package flymake
  :ensure nil
  ;; lsp-mode enables Flymake for LSP-managed buffers.  Do not enable it in
  ;; every prog-mode buffer, which would also run standalone checkers such as
  ;; `rust-ts-flymake` alongside the language server.
  :custom
  (flymake-indicator-type 'fringes)
  (flymake-fringe-indicator-position 'right-fringe)
  (flymake-margin-indicator-position 'right-margin)
  (flymake-error-bitmap '(flymake-double-exclamation-mark flymake-error-fringe))
  (flymake-warning-bitmap '(exclamation-mark flymake-warning-fringe))
  (flymake-note-bitmap '(exclamation-mark flymake-note-fringe)))

;;-----------------------------------------------------------------------------
;; LSP and Language Modes
;;-----------------------------------------------------------------------------
(use-package envrc
  :init
  ;; envrc disables TRAMP support unless explicitly enabled.
  (setq envrc-remote t)
  :hook (after-init . envrc-global-mode)
  :config
  (add-to-list 'envrc-supported-tramp-methods "sshx"))

(with-eval-after-load 'tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (add-to-list 'tramp-remote-path "~/.nix-profile/bin"))

;; (use-package direnv
;;   :config
;;   (direnv-mode))

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

(defvar my--lsp-help-just-opened nil
  "Non-nil when lsp-mode displayed *lsp-help* during the current command.")

(with-eval-after-load 'lsp-mode
  (advice-add 'lsp--display-contents :after
              (lambda (&rest _)
                (setq my--lsp-help-just-opened t))))

(defun my-close-lsp-help-buffer-if-left ()
  "Close *lsp-help* after a subsequent command outside its window."
  (cond
   ((eq (current-buffer) (get-buffer "*lsp-help*"))
    (setq my--lsp-help-just-opened nil))
   (my--lsp-help-just-opened
    (setq my--lsp-help-just-opened nil))
   (t
    (when-let* ((window (get-buffer-window "*lsp-help*")))
      (quit-window t window)))))
(add-hook 'post-command-hook #'my-close-lsp-help-buffer-if-left)

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

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (rust-ts-mode . lsp-deferred)
         (rust-mode . lsp-deferred)
         (nix-mode . lsp-deferred)
         (lua-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)

  :config
  (setq lsp-enable-snippet t)
  (setq lsp-enable-folding t)
  (setq lsp-semantic-tokens-enable t)
  (setq lsp-enable-imenu t)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-modeline-diagnostics-enable t)
  (setq lsp-modeline-workspace-status-enable t)
  (setq lsp-enable-symbol-highlighting t)
  (setq lsp-enable-links t)
  (setq lsp-enable-indentation t)
  (setq lsp-enable-on-type-formatting t)
  (setq lsp-before-save-edits t)
  (setq lsp-format-buffer-on-save nil)
  (setq lsp-format-buffer-on-save-list '(python-mode rust-mode rust-ts-mode))
  (setq lsp-diagnostics-provider :flymake)
  (setq lsp-diagnostic-clean-after-change t)
  (setq lsp-completion-provider :none) ;; Using Corfu via CAPF
  (setq lsp-eldoc-enable-hover t)
  (setq lsp-eldoc-render-all nil)

  (defun my-lsp-use-only-flymake-backend ()
    (when (memq 'lsp-diagnostics--flymake-backend
                flymake-diagnostic-functions)
      (setq-local flymake-diagnostic-functions
                  '(lsp-diagnostics--flymake-backend))))
  ;; Append so lsp-mode has first installed its own Flymake backend.
  (add-hook 'lsp-configure-hook #'my-lsp-use-only-flymake-backend t)
  )

;;-----------------------------------------------------------------------------
;; Language Modes
;;-----------------------------------------------------------------------------
(font-lock-mode 1)
(defun my-markdown-faces ()
  (set-face-attribute 'markdown-ts-heading-1 nil :height 1.8 :family my/font-variable-family)
  (set-face-attribute 'markdown-ts-heading-2 nil :height 1.6 :family my/font-variable-family)
  (set-face-attribute 'markdown-ts-heading-3 nil :height 1.4 :family my/font-variable-family)
  (set-face-attribute 'markdown-ts-heading-4 nil :height 1.2 :family my/font-variable-family)
  (set-face-attribute 'markdown-ts-heading-5 nil :height 1.1 :family my/font-variable-family)
  (set-face-attribute 'markdown-code-face nil :height 1.0 :family my/font-fixed-family)
  (set-face-attribute 'markdown-inline-code-face nil :height 1.0 :family my/font-fixed-family)
  (set-face-attribute 'markdown-table-face nil :height 1.0 :family my/font-fixed-family)
  )
(add-hook 'markdown-ts-mode-hook #'my-markdown-faces)

;;-----------------------------------------------------------------------------
;; Git Integration
;;-----------------------------------------------------------------------------
(use-package transient
  :ensure t
  :defer t)
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
  :config
  (evil-define-key 'normal diff-hl-mode-map (kbd "[c") #'diff-hl-previous-hunk)
  (evil-define-key 'normal diff-hl-mode-map (kbd "]c") #'diff-hl-next-hunk)
  (defun my/diff-hl-preview-enable-evil-keys (&rest _)
    (let ((preview (get-buffer diff-hl-show-hunk-buffer-name)))
      (when preview
        (with-current-buffer preview
          (dolist (state '(normal motion))
            (evil-local-set-key state (kbd "[c") #'diff-hl-show-hunk-previous)
            (evil-local-set-key state (kbd "]c") #'diff-hl-show-hunk-next)
            (evil-local-set-key state (kbd "n") #'diff-hl-show-hunk-next)
            (evil-local-set-key state (kbd "p") #'diff-hl-show-hunk-previous)
            (evil-local-set-key state (kbd "r") #'diff-hl-show-hunk-revert-hunk)
            (evil-local-set-key state (kbd "c") #'diff-hl-show-hunk-copy-original-text)
            (evil-local-set-key state (kbd "S") #'diff-hl-show-hunk-stage-hunk))))))
  (with-eval-after-load 'diff-hl-show-hunk
    (advice-remove #'diff-hl-show-hunk-inline-popup
                   #'my/diff-hl-preview-enable-evil-keys)
    (advice-add #'diff-hl-show-hunk-inline-popup :after
                #'my/diff-hl-preview-enable-evil-keys))
  (global-diff-hl-mode 1)
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

;;-----------------------------------------------------------------------------
;; Leader and global keybindings
;;-----------------------------------------------------------------------------
(use-package general
  :ensure t
  :config
  (general-create-definer my/leader-def
    :states '(normal visual)
    :prefix "SPC")

  ;; All former C-c bindings live under the Space leader in Evil states.
  (my/leader-def
    "." '(find-file :which-key "find file")
    "-" '((lambda () (interactive) (dired default-directory)) :which-key "dired here")

    "a" '(:ignore t :which-key "actions")
    "a a" '(embark-act :which-key "act")
    "a d" '(embark-dwim :which-key "DWIM")
    "i" '(:ignore t :which-key "Pi tmux")
    "i a" '(pi-tmux-attach :which-key "attach")
    "i d" '(pi-tmux-detach :which-key "detach")
    "i f" '(pi-tmux-focus :which-key "focus")
    "i r" '(pi-tmux-send-region :which-key "send region")
    "i c" '(pi-tmux-send-context :which-key "send context")
    "i p" '(pi-tmux-send-text :which-key "send prompt")

    "p" '(:keymap project-prefix-map :which-key "project")

    "b" '(:ignore t :which-key "buffers")
    "b b" '(consult-buffer :which-key "switch buffer")
    "b k" '(kill-this-buffer :which-key "kill buffer")
    "b i" '(ibuffer :which-key "ibuffer")
    "b n" '(next-buffer :which-key "next buffer")
    "b p" '(previous-buffer :which-key "previous buffer")
    "b r" '(revert-buffer :which-key "revert buffer")
    "b j" '(consult-bookmark :which-key "bookmarks")

    "f" '(:ignore t :which-key "find")
    "f c" '((lambda () (interactive) (find-file "~/.config/emacs/init.el")) :which-key "Emacs config")
    "f r" '(consult-recent-file :which-key "recent file")
    "f f" '(consult-fd :which-key "find file")
    "f t" '(consult-ripgrep :which-key "find text")
    "f l" '(consult-line :which-key "find line")

    "g" '(:ignore t :which-key "git")
    "g g" '(magit-status :which-key "status")
    "g l" '(magit-log-current :which-key "log current")
    "g d" '(magit-diff-buffer-file :which-key "diff current")
    "g p" '(diff-hl-show-hunk :which-key "show hunk")
    "g s" '(diff-hl-stage-current-hunk :which-key "stage hunk")
    "g r" '(diff-hl-revert-hunk :which-key "revert hunk")
    "g v" '(:ignore t :which-key "version control")
    "g v d" '(vc-dir :which-key "directory")
    "g v b" '(vc-annotate :which-key "annotate")
    "g v =" '(vc-diff :which-key "diff current")
    "g v D" '(vc-root-diff :which-key "diff project")
    "g v v" '(vc-next-action :which-key "next action")

    "j" '(:ignore t :which-key "jump")
    "j c" '(avy-goto-char :which-key "character")
    "j j" '(avy-goto-line-below :which-key "line below")
    "j k" '(avy-goto-line-above :which-key "line above")

    "l" '(:ignore t :which-key "LSP")
    "l k" '(lsp-describe-thing-at-point :which-key "documentation")
    "l f" '(lsp-format-buffer :which-key "format buffer")
    "l d" '(lsp-find-definition :which-key "definition")
    "l r" '(lsp-find-references :which-key "references")
    "l c" '(lsp-find-declaration :which-key "declaration")
    "l i" '(lsp-find-implementation :which-key "implementation")
    "l D" '(:ignore t :which-key "document")
    "l D s" '(consult-imenu :which-key "document symbols")
    "l D d" '(consult-flymake :which-key "document diagnostics")
    "l w" '(:ignore t :which-key "workspace")
    "l w s" '(consult-lsp-file-symbols :which-key "workspace symbols")
    "l w d" '(consult-flymake :which-key "workspace diagnostics")

    "m" '(:ignore t :which-key "multiple cursors")
    "m a" '(mc/mark-all-like-this :which-key "mark all like this")

    "s" '(:ignore t :which-key "merge and surround")
    "s u" '(smerge-keep-upper :which-key "keep upper")
    "s l" '(smerge-keep-lower :which-key "keep lower")
    "s n" '(smerge-next :which-key "next conflict")
    "s p" '(smerge-previous :which-key "previous conflict")

    "t" '(:ignore t :which-key "toggles")
    "t t" '(visual-line-mode :which-key "visual line")
    "t l" '(display-line-numbers-mode :which-key "line numbers")

    "w" '(:ignore t :which-key "windows")
    "w h" '(windmove-left :which-key "move left")
    "w j" '(windmove-down :which-key "move down")
    "w k" '(windmove-up :which-key "move up")
    "w l" '(windmove-right :which-key "move right")
    "w H" '(shrink-window-horizontally :which-key "shrink width")
    "w J" '(shrink-window :which-key "shrink height")
    "w K" '(enlarge-window :which-key "enlarge height")
    "w L" '(enlarge-window-horizontally :which-key "enlarge width")
    "w s" '(split-window-below :which-key "split below")
    "w v" '(split-window-right :which-key "split right")
    "w d" '(delete-window :which-key "delete window")
    "w o" '(delete-other-windows :which-key "delete others")
    "w w" '(other-window :which-key "other window")

    "z" '(:ignore t :which-key "folding")
    "z a" '(hs-toggle-hiding :which-key "toggle")
    "z c" '(hs-hide-block :which-key "hide block")
    "z o" '(hs-show-block :which-key "show block")
    "z R" '(hs-show-all :which-key "show all")
    "z M" '(hs-hide-all :which-key "hide all"))

  ;; Magit uses Evil's motion state.  Reuse the normal-state leader map there
  ;; so the Space commands remain available in Magit and other motion buffers.
  (evil-define-key 'motion 'global (kbd "SPC")
    (lookup-key evil-normal-state-map (kbd "SPC")))

  (general-def
    "C-<next>" '(scroll-up-line :which-key "scroll up line")
    "C-<prior>" '(scroll-down-line :which-key "scroll down line")
    "C-M-j" '(scroll-up-line :which-key "scroll up line")
    "C-M-k" '(scroll-down-line :which-key "scroll down line")
    "M-S-<right>" '(enlarge-window-horizontally :which-key "window width increase")
    "M-S-<left>" '(shrink-window-horizontally :which-key "window width decrease")
    "M-S-<up>" '(enlarge-window :which-key "window height increase")
    "M-S-<down>" '(shrink-window :which-key "window height decrease")
    "M-L" '(enlarge-window-horizontally :which-key "window width increase")
    "M-H" '(shrink-window-horizontally :which-key "window width decrease")
    "M-J" '(enlarge-window :which-key "window height increase")
    "M-K" '(shrink-window :which-key "window height decrease"))

  (general-def
    :prefix "C-x"
    "_" '(split-window-below :which-key "split below")
    "|" '(split-window-right :which-key "split right")))

(require 'pi-tmux-sessions)

;;; init.el ends here
