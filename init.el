;;; init.el --- My Emacs Config
;; Author: Junior Sundar
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0"))

(setq gc-cons-threshold #x40000000)
;; Increase the amount of data which Emacs reads from the process
(setq read-process-output-max (* 1024 1024 4))

;;-----------------------------------------------------------------------------
;; Set up use-package and add possibility for custom configs
;;-----------------------------------------------------------------------------
(require 'use-package-ensure) ;; Load use-package-always-ensure
(setq use-package-always-ensure t) ;; Always ensures that a package is installed
(setq package-archives '(("melpa" . "https://melpa.org/packages/") ;; Sets default package repositories
						 ("org" . "https://orgmode.org/elpa/")
						 ("elpa" . "https://elpa.gnu.org/packages/")
						 ("nongnu" . "https://elpa.nongnu.org/nongnu/"))) ;; For Eat Terminal

(defvar my-config-dir (expand-file-name "lisp" user-emacs-directory)
  "Directory containing my configuration files.")
(defun load-config-file (file)
  (load (expand-file-name file my-config-dir)))

;;-----------------------------------------------------------------------------
;; Add to search paths
;;-----------------------------------------------------------------------------
(add-to-list 'exec-path "/usr/local/go/bin")
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "~/.local/bin")
(add-to-list 'exec-path "~/go/bin")
(add-to-list 'exec-path "/usr/bin")
(add-to-list 'exec-path "~/anaconda3/bin")
(add-to-list 'exec-path "~/.nvm/versions/node/v20.15.0/bin/")
;; (add-to-list 'exec-path (expand-file-name "~/.local/share/nvim/mason/bin"))

;;-----------------------------------------------------------------------------
;; Default Emacs Configurations
;;-----------------------------------------------------------------------------
(use-package emacs
  :ensure nil
  :custom
  ;; General Settings
  ;; (column-number-mode t)                          ;; Display the column number in the mode line.
  (auto-save-default nil)                         ;; Disable automatic saving of buffers.
  (create-lockfiles nil)                          ;; Prevent the creation of lock files when editing.
  (delete-by-moving-to-trash t)                   ;; Move deleted files to the trash instead of permanently deleting them.
  (delete-selection-mode t)                       ;; Enable replacing selected text with typed text.
  (display-line-numbers-type 'relative)           ;; Use relative line numbering.
  (global-display-line-numbers-mode t)            ;; Enable line numbers globally.
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
  ;; UI Settings
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
  ;; Function to skip special buffers when navigating
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
  (prog-mode . display-line-numbers-mode)         ;; Enable line numbers in programming modes.
  (prog-mode . hs-minor-mode)                    ;; Enable code folding in programming modes.
  )

(savehist-mode) ;; Enables save history mode

(use-package eat
  :hook ('eshell-load-hook #'eat-eshell-mode))

(use-package eldoc
  :ensure nil          ;; This is built-in, no need to fetch it.
  :init
  (global-eldoc-mode))


(use-package dired
  :ensure nil                                                ;; This is built-in, no need to fetch it.
  :custom
  (dired-listing-switches "-lah --group-directories-first")  ;; Display files in a human-readable format and group directories first.
  (dired-dwim-target t)                                      ;; Enable "do what I mean" for target directories.
  (dired-guess-shell-alist-user
   '(("\\.\\(png\\|jpe?g\\|tiff\\)" "feh" "xdg-open" "open") ;; Open image files with `feh' or the default viewer.
     ("\\.\\(mp[34]\\|m4a\\|ogg\\|flac\\|webm\\|mkv\\)" "mpv" "xdg-open" "open") ;; Open audio and video files with `mpv'.
     (".*" "open" "xdg-open")))                              ;; Default opening command for other files.
  (dired-kill-when-opening-new-dired-buffer t)               ;; Close the previous buffer when opening a new `dired' instance.
  :config
  (when (eq system-type 'darwin)
    (let ((gls (executable-find "gls")))                     ;; Use GNU ls on macOS if available.
      (when gls
        (setq insert-directory-program gls)))))

(use-package flymake
  :ensure nil          ;; This is built-in, no need to fetch it.
  :defer t
  :hook (prog-mode . flymake-mode)
  :custom
  (flymake-margin-indicators-string
   '((error "!»" compilation-error) (warning "»" compilation-warning)
	 (note "»" compilation-info))))

(use-package which-key
  :init
  (which-key-mode 1)
  :diminish
  :custom
  (which-key-side-window-location 'bottom)
  (which-key-sort-order #'which-key-key-order-alpha) ;; Same as default, except single characters are sorted alphabetically
  (which-key-sort-uppercase-first nil)
  (which-key-add-column-padding 1) ;; Number of spaces to add to the left of each column
  (which-key-min-display-lines 6)  ;; Increase the minimum lines to display, because the default is only 1
  (which-key-idle-delay 0.8)       ;; Set the time delay (in seconds) for the which-key popup to appear
  (which-key-max-description-length 25)
  (which-key-allow-imprecise-window-fit nil)) ;; Fixes which-key window slipping out in Emacs Daemon

(use-package window
  :ensure nil       ;; This is built-in, no need to fetch it.
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

     ;; Example configuration for the LSP help buffer,
     ;; keeps it always on bottom using 25% of the available space:
     ("\\*\\(lsp-help\\)\\*"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 0))
     
     ;; Configuration for displaying various diagnostic buffers on
     ;; bottom 25%:
     ("\\*\\(Flymake diagnostics\\|xref\\|ivy\\|Swiper\\|Completions\\)"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 1))
	 )))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package diminish)

;;-----------------------------------------------------------------------------
;; Evil
;;-----------------------------------------------------------------------------
(use-package evil
  :init ;; Execute code Before a package is loaded
  (evil-mode)
  :config ;; Execute code After a package is loaded
  (evil-set-initial-state 'eat-mode 'insert) ;; Set initial state in eat terminal to insert mode
  :custom ;; Customization of package custom variables
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
  ;; Bindings for VISUAL state (newly integrated)
  )

(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-collection
  :ensure t
  :after evil ;; Ensure evil is loaded first
  :config
  ;; Just initialize it. It will automatically set up integrations
  ;; for all supported packages it finds (like dired, magit, corfu, vertico, etc.)
  ;; No need to set evil-collection-mode-list unless you specifically want to *exclude* modes.
  (evil-collection-init))

(use-package avy
  :ensure t
  )

(use-package evil-avy
  :ensure t
  :after (evil avy) ;; Ensure both are loaded first
  :config
  (evil-define-key '(normal visual) 'global "gy" #'avy-goto-char-timer)
  (evil-define-key '(normal visual) 'global "gl" #'avy-goto-line)
  (evil-define-key '(normal visual) 'global "gw" #'avy-goto-word-1)
  (general-define-key
   :keymaps 'evil-normal-state-map
   "SPC j c" 'avy-goto-char-timer
   "SPC j l" 'avy-goto-line
   "SPC j w" 'avy-goto-word-1
   )
  (general-define-key
   :keymaps 'evil-visual-state-map
   "SPC j c" 'avy-goto-char-timer
   "SPC j l" 'avy-goto-line
   "SPC j w" 'avy-goto-word-1
   )
  )

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
  ;; Global settings (defaults)
  (doom-themes-enable-bold t)   ; if nil, bold is universally disabled
  (doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; for treemacs users
  (doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  :config
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (nerd-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))
;;-----------------------------------------------------------------------------
;; Fonts
;;-----------------------------------------------------------------------------
(use-package fontaine
  :ensure t)
(setq fontaine-presets
      '((default
         :default-family "Monaspace Neon"
         :default-weight regular
         :default-height 120
         :fixed-pitch-family "Monaspace Neon"
         :fixed-pitch-weight regular
         :italic-family "Monaspace Argon"
         :italic-slant italic
         :variable-pitch-family "SF Pro"
         :variable-pitch-weight regular
         :variable-pitch-height 140)))
;; Set the default preset
(fontaine-set-preset 'default)
(setq-default line-spacing 0.2)

;; Set Nerd Font for symbols
(let ((font-spec (font-spec :family "Symbols Nerd Font Mono" :size 20)))
  (set-fontset-font t 'unicode font-spec nil 'prepend)
  (set-fontset-font t '(#x1F000 . #x1F02F) font-spec)  ;; Mahjong Tiles
  (set-fontset-font t '(#x1F0A0 . #x1F0FF) font-spec)  ;; Playing Cards
  (set-fontset-font t '(#x1F300 . #x1F5FF) font-spec)  ;; Misc Symbols and Pictographs
  (set-fontset-font t '(#x1F600 . #x1F64F) font-spec)  ;; Emoticons
  (set-fontset-font t '(#x1F680 . #x1F6FF) font-spec)  ;; Transport and Map
  (set-fontset-font t '(#x1F700 . #x1F77F) font-spec)  ;; Alchemical Symbols
  (set-fontset-font t '(#x1F780 . #x1F7FF) font-spec)  ;; Geometric Shapes Extended
  (set-fontset-font t '(#x1F800 . #x1F8FF) font-spec)  ;; Supplemental Arrows-C
  (set-fontset-font t '(#x1F900 . #x1F9FF) font-spec)  ;; Supplemental Symbols and Pictographs
  (set-fontset-font t '(#x1FA00 . #x1FA6F) font-spec)  ;; Chess Symbols
  (set-fontset-font t '(#x1FA70 . #x1FAFF) font-spec)  ;; Symbols and Pictographs Extended-A
  (set-fontset-font t '(#x2600 . #x26FF) font-spec)    ;; Miscellaneous Symbols
  (set-fontset-font t '(#x2700 . #x27BF) font-spec))  ;; Dingbats

;; Ensure the italic face is only italic and not underlined
(set-face-attribute 'italic nil
                    :underline nil
                    :slant 'italic
                    :family "Monaspace Argon")

(add-hook 'org-mode-hook 'variable-pitch-mode)
(add-hook 'markdown-mode-hook 'variable-pitch-mode)
(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode -1)))
(add-hook 'markdown-mode-hook (lambda () (display-line-numbers-mode -1)))

(use-package all-the-icons
  :ensure t)

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package ligature
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia and Fira Code ligatures in programming modes
  (ligature-set-ligatures 't
						  '(;; == === ==== => =| =>>=>=|=>==>> ==< =/=//=// =~
							;; =:= =!=
							("=" (rx (+ (or ">" "<" "|" "/" "~" ":" "!" "="))))
							;; ;; ;;;
							(";" (rx (+ ";")))
							;; && &&&
							("&" (rx (+ "&")))
							;; !! !!! !. !: !!. != !== !~
							("!" (rx (+ (or "=" "!" "\." ":" "~"))))
							;; ?? ??? ?:  ?=  ?.
							("?" (rx (or ":" "=" "\." (+ "?"))))
							;; %% %%%
							("%" (rx (+ "%")))
							;; |> ||> |||> ||||> |] |} || ||| |-> ||-||
							;; |->>-||-<<-| |- |== ||=||
							;; |==>>==<<==<=>==//==/=!==:===>
							("|" (rx (+ (or ">" "<" "|" "/" ":" "!" "}" "\]"
											"-" "=" ))))
							;; \\ \\\ \/
							("\\" (rx (or "/" (+ "\\"))))
							;; ++ +++ ++++ +>
							("+" (rx (or ">" (+ "+"))))
							;; :: ::: :::: :> :< := :// ::=
							(":" (rx (or ">" "<" "=" "//" ":=" (+ ":"))))
							;; // /// //// /\ /* /> /===:===!=//===>>==>==/
							("/" (rx (+ (or ">"  "<" "|" "/" "\\" "\*" ":" "!"
											"="))))
							;; .. ... .... .= .- .? ..= ..<
							("\." (rx (or "=" "-" "\?" "\.=" "\.<" (+ "\."))))
							;; -- --- ---- -~ -> ->> -| -|->-->>->--<<-|
							("-" (rx (+ (or ">" "<" "|" "~" "-"))))
							;; *> */ *)  ** *** ****
							("*" (rx (or ">" "/" ")" (+ "*"))))
							;; www wwww
							("w" (rx (+ "w")))
							;; <> <!-- <|> <: <~ <~> <~~ <+ <* <$ </  <+> <*>
							;; <$> </> <|  <||  <||| <|||| <- <-| <-<<-|-> <->>
							;; <<-> <= <=> <<==<<==>=|=>==/==//=!==:=>
							;; << <<< <<<<
							("<" (rx (+ (or "\+" "\*" "\$" "<" ">" ":" "~"  "!"
											"-"  "/" "|" "="))))
							;; >: >- >>- >--|-> >>-|-> >= >== >>== >=|=:=>>
							;; >> >>> >>>>
							(">" (rx (+ (or ">" "<" "|" "/" ":" "=" "-"))))
							;; #: #= #! #( #? #[ #{ #_ #_( ## ### #####
							("#" (rx (or ":" "=" "!" "(" "\?" "\[" "{" "_(" "_"
										 (+ "#"))))
							;; ~~ ~~~ ~=  ~-  ~@ ~> ~~>
							("~" (rx (or ">" "=" "-" "@" "~>" (+ "~"))))
							;; __ ___ ____ _|_ __|____|_
							("_" (rx (+ (or "_" "|"))))
							;; Fira code: 0xFF 0x12
							("0" (rx (and "x" (+ (in "A-F" "a-f" "0-9")))))
							;; Fira code:
							"Fl"  "Tl"  "fi"  "fj"  "fl"  "ft"
							;; The few not covered by the regexps.
							"{|"  "[|"  "]#"  "(*"  "}#"  "$>"  "^="))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

;;-----------------------------------------------------------------------------
;; Modeline
;;-----------------------------------------------------------------------------
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 25)     ;; Sets modeline height
  (doom-modeline-bar-width 5)   ;; Sets right bar width
  (doom-modeline-persp-name t)  ;; Adds perspective name to modeline
  (doom-modeline-persp-icon t)
  (doom-modeline-enable-word-count t)) ;; Adds folder icon next to persp name

;;-----------------------------------------------------------------------------
;; Projectile
;;-----------------------------------------------------------------------------
(use-package projectile
  :init
  (projectile-mode)
  :custom
  (projectile-run-use-comint-mode t) ;; Interactive run dialog when running projects inside emacs (like giving input)
  (projectile-switch-project-action #'projectile-dired) ;; Open dired when switching to a project
  (projectile-project-search-path '("~/Documents/Projects/"
									;; "~/Documents/work/"
									)
								  )
  ) ;; . 1 means only search the first subdirectory level for projects
;; Use Bookmarks for smaller, not standard projects

;;-----------------------------------------------------------------------------
;; LSP and Language Modes
;;-----------------------------------------------------------------------------
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "SPC L")
  :ensure t
  :config
  ;; (setq lsp-keymap-prefix "C-l")
  (lsp-register-custom-settings
   '(("pyls.plugins.pyls_mypy.enabled" t t)
	 ("pyls.plugins.pyls_mypy.live_mode" nil t)
	 ("pyls.plugins.pyls_black.enabled" t t)
	 ("pyls.plugins.pyls_isort.enabled" t t)))
  (setq lsp-gopls-server-path (executable-find "gopls"))
  :hook (
		 (go-ts-mode . lsp)
		 (python-ts-mode . lsp)
		 (zig-ts-mode . lsp)
		 (rust-ts-mode . lsp)
		 (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  :custom
  (lsp-inlay-hint-enable t)                             ;; Enable inlay hints.
  (lsp-completion-provider :none)                       ;; Disable the default completion provider.
  (lsp-session-file (locate-user-emacs-file ".lsp-session")) ;; Specify session file location.
  (lsp-log-io nil)                                      ;; Disable IO logging for speed.
  (lsp-idle-delay 0.1)                                    ;; Set the delay for LSP to 0 (debouncing).
  (lsp-keep-workspace-alive nil)                        ;; Disable keeping the workspace alive.
  ;; Core settings
  (lsp-enable-xref t)                                   ;; Enable cross-references.
  (lsp-auto-configure t)                                ;; Automatically configure LSP.
  (lsp-enable-links nil)                                ;; Disable links.
  (lsp-eldoc-enable-hover t)                            ;; Enable ElDoc hover.
  (lsp-enable-file-watchers nil)                        ;; Disable file watchers.
  (lsp-enable-folding t)                              ;; Disable folding.
  (lsp-enable-imenu t)                                  ;; Enable Imenu support.
  (lsp-enable-indentation t)                          ;; Disable indentation.
  (lsp-enable-on-type-formatting nil)                   ;; Disable on-type formatting.
  (lsp-enable-suggest-server-download t)                ;; Enable server download suggestion.
  (lsp-enable-symbol-highlighting t)                    ;; Enable symbol highlighting.
  (lsp-enable-text-document-color t)                  ;; Disable text document color.
  ;; Modeline settings
  (lsp-modeline-code-actions-enable t)                ;; Keep modeline clean.
  (lsp-modeline-diagnostics-enable t)                 ;; Use `flymake' instead.
  (lsp-modeline-workspace-status-enable t)              ;; Display "LSP" in the modeline when enabled.
  (lsp-signature-doc-lines 1)                           ;; Limit echo area to one line.
  (lsp-eldoc-render-all nil)                              ;; Render all ElDoc messages.
  ;; Completion settings
  (lsp-completion-enable t)                             ;; Enable completion.
  (lsp-completion-enable-additional-text-edit t)        ;; Enable additional text edits for completions.
  (lsp-enable-snippet t)                              ;; Disable snippets
  (lsp-completion-show-kind t)                          ;; Show kind in completions.
  ;; Lens settings
  (lsp-lens-enable t)                                   ;; Enable lens support.
  ;; Headerline settings
  (lsp-headerline-breadcrumb-enable-symbol-numbers t)   ;; Enable symbol numbers in the headerline.
  (lsp-headerline-arrow "▶")                            ;; Set arrow for headerline.
  (lsp-headerline-breadcrumb-enable-diagnostics nil)    ;; Disable diagnostics in headerline.
  (lsp-headerline-breadcrumb-icons-enable nil)          ;; Disable icons in breadcrumb.
  ;; Semantic settings
  (lsp-semantic-tokens-enable t)
  :bind (:map lsp-mode-map ;; Bind only when lsp-mode is active
              ("C-l d" . consult-flymake))
  )

(use-package lsp-ui
  :ensure t
  :config
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-position 'top)
  (setq lsp-ui-doc-side 'right)
  (setq lsp-ui-doc-show-with-cursor t)
  (setq lsp-ui-sideline-enable t) ; Sane default
  (setq lsp-ui-sideline-show-code-actions t)
  )

(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
(add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode))
(add-to-list 'major-mode-remap-alist '(zig-mode . zig-ts-mode))
(add-to-list 'major-mode-remap-alist '(rust-mode . rust-ts-mode))

;; (add-hook 'go-mode-hook #'lsp-deferred)
;; ;; Set up before-save hooks to format buffer and add/delete imports.
;; ;; Make sure you don't have other gofmt/goimports hooks enabled.
;; (defun lsp-go-install-save-hooks ()
;;   (add-hook 'before-save-hook #'lsp-format-buffer t t)
;;   (add-hook 'before-save-hook #'lsp-organize-imports t t))
;; (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

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
;; Org-Mode
;;-----------------------------------------------------------------------------
(use-package org
  :ensure t
  :hook
  (org-mode . org-indent-mode) ;; Indent text
  (org-mode . visual-line-mode)
  :custom
  (org-return-follows-link t))

;; Ensure inline images are displayed when opening an Org file
(setq org-startup-with-inline-images t)
(setq org-image-actual-width 500) 

;; Set Org directory
(setq org-directory "~/Dropbox/org/")

;; Recursive function to find all .org files in a directory
(defun my/org-agenda-files-recursive (directory)
  "Recursively find all .org files in DIRECTORY."
  (directory-files-recursively directory "\\.org$"))
;; Recursive function to find all .org files in a directory
(defun my/org-agenda-files-recursive (directory)
  "Recursively find all .org files in DIRECTORY."
  (directory-files-recursively directory "\\.org$"))
;; Set org-agenda-files dynamically before running org-agenda
(defun my/update-org-agenda-files (&rest _)
  "Update `org-agenda-files` to include all .org files in the directory."
  (setq org-agenda-files (my/org-agenda-files-recursive "~/Dropbox/org/")))

;; Ensure the agenda files are updated before calling the agenda
(advice-add 'org-agenda :before #'my/update-org-agenda-files)

(defun my/kill-agenda-file-buffers ()
  "Kill unmodified buffers visiting files listed in `org-agenda-files`."
  (interactive)
  (let ((killed-count 0)
        ;; Ensure agenda files list is current (or rely on static setting)
        (agenda-files (org-agenda-files t)) ; Pass 't' to maybe refresh if needed? Or just use the variable value.
        (agenda-files-set (make-hash-table :test 'equal)))
    ;; Create a hash set for quick lookup
    (dolist (file agenda-files)
      (puthash (expand-file-name file) t agenda-files-set))

    (dolist (buffer (buffer-list))
      (let ((filename (buffer-file-name buffer)))
        (when (and filename
                   (gethash (expand-file-name filename) agenda-files-set) ; Is it an agenda file?
                   (not (buffer-modified-p buffer)) ; Is it unmodified?
                   (not (eq buffer (current-buffer))) ; Is it not the current buffer?
                   ;; Add check to avoid killing special org buffers? Maybe check name?
                   (not (string-prefix-p "*" (buffer-name buffer))))
          (kill-buffer buffer)
          (setq killed-count (1+ killed-count)))))
    (message "Killed %d agenda file buffers." killed-count)))

(setq org-todo-keywords
      '((sequence "TODO(t)" "DOING(d!)" "HOLD(h)" "|" "DONE(D)" "CANCELLED(c)" "MAYBE(m)")))
(setq org-todo-keyword-faces
      '(("DOING" . (:background "orange" :foreground "black"))
        ("DONE" . (:background "green" :foreground "black"))
        ("HOLD" . (:background "turquoise" :foreground "black"))
        ("TODO" . (:background "red" :foreground "white"))
        ("CANCELLED" . (:background "gray" :foreground "black"))
        ("MAYBE" . (:background "yellow" :foreground "black"))))
;; Customize agenda prefix format
(setq org-agenda-prefix-format
      '((agenda . " %i %?-12t% s")  ; remove file name
        (todo . " %i ")
        (tags . " %i ")
        (search . " %i ")))

;; This is to create highlighted org-admonitions
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
    ;; (propertize " " 'face face 'display '(space :align-to 0)))
    (overlay-put overlay 'after-string
                 (propertize " " 'face face 'display '(space :align-to right-margin)))))
(defun highlight-org-block-region ()
  "Highlight org blocks with full window width background."
  (remove-overlays (point-min) (point-max)) ; Clear existing overlays
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
(setq org-log-done 'time)

;; Set default notes file
(setq org-default-notes-file (concat org-directory "/inbox.org"))

(setq org-attach-id-dir "~/Dropbox/org/assets/")
(setq org-attach-use-inheritance t)

;; Define capture templates
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

;; Conceal emphasis markers for bold and italic text
(setq org-hide-emphasis-markers t)

;; Customize the appearance of inline code #45475a #c6d0f5
(custom-set-faces
 '(org-code ((t (:background "#1e2124" :foreground "#ffffff" :family "Monaspace Neon")))))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/Dropbox/org/pages"))
  (org-roam-dailies-directory "./journals/")
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
	  ;; Accomodates for the fact that Logseq uses the "pages" directory
	  :target (file+head "pages/${slug}.org" "#+title: ${title}\n")
	  :unnarrowed t))
   org-roam-dailies-capture-templates
   '(("d" "default" entry "* %?"
	  :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n#+FILETAGS: journal"))))


  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(use-package toc-org
  :commands toc-org-enable
  :hook (org-mode . toc-org-mode))

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :config
  ;; (setq org-superstar-headline-bullets-list '("󰼏" "󰼐" "󰼑" "󰼒" "󰼓" "󰼔"))
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

;;-----------------------------------------------------------------------------
;; Git Integration
;;-----------------------------------------------------------------------------
(use-package magit
  :ensure t ;; Ensure package is installed
  :commands (magit-status magit-blame-addition) 
  )

(use-package diff-hl
  :defer t
  :ensure t
  :hook ((find-file . diff-hl-mode)                ;; Enable Diff-HL on file open.
         (after-save . diff-hl-update)             ;; Update diffs after save.
         (vc-dir-mode . diff-hl-dir-mode)          ;; Enable in version control directories.
         (dired-mode . diff-hl-dired-mode))        ;; Enable in Dired mode.
  :init
  (global-diff-hl-mode 1)                            ;; Enable Diff-HL globally.
  :config
  (diff-hl-flydiff-mode 1)                           ;; Auto-refresh diffs.
  (diff-hl-margin-mode 1)                            ;; Show indicators in the margin.
  :custom
  (diff-hl-side 'left)                             ;; Set the side for diff indicators.
  (diff-hl-margin-symbols-alist '((insert . "│")   ;; Customize symbols for each change type.
                                  (delete . "-")
                                  (change . "│")
                                  (unknown . "?")
                                  (ignored . "i"))))

;; -----------------------------------------------------------------------------
;; Completions
;;-----------------------------------------------------------------------------
(use-package corfu
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-prefix 2)          ;; Minimum length of prefix for auto completion.
  (corfu-separator ?\s)          ;; Orderless field separator, Use M-SPC to enter separator
  (corfu-quit-at-boundary 'separator)   ;; Never quit at completion boundary
  (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-popupinfo-mode t)       ;; Enable popup information
  (corfu-popupinfo-delay 0.5)    ;; Lower popupinfo delay to 0.5 seconds from 2 seconds
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin
  (completion-ignore-case t)
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))

(use-package cape
  :after corfu
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  ;; The functions that are added later will be the first in the list

  (add-to-list 'completion-at-point-functions #'cape-dabbrev nil t) ;; Complete word from current buffers
  (add-to-list 'completion-at-point-functions #'cape-dict nil t) ;; Dictionary completion
  (add-to-list 'completion-at-point-functions #'cape-file nil t) ;; Path completion
  (add-to-list 'completion-at-point-functions #'cape-elisp-block nil t) ;; Complete elisp in Org or Markdown mode
  (add-to-list 'completion-at-point-functions #'cape-keyword nil t) ;; Keyword/Snipet completion

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
  ;; (orderless-style-dispatchers '(orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package vertico
  :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

(use-package marginalia
  :after vertico
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
			  ("M-A" . marginalia-cycle))
  ;; The :init section is always executed.
  :init
  (marginalia-mode))

;;-----------------------------------------------------------------------------
;; Search
;;-----------------------------------------------------------------------------

(use-package consult
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
		register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
		xref-show-definitions-function #'consult-xref)
  :config
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-project-function nil)
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))

  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
  consult-theme :preview-key '(:debounce 0.2 any)
  consult-ripgrep consult-git-grep consult-grep consult-fd
  consult-bookmark consult-recent-file consult-xref
  consult--source-bookmark consult--source-file-register
  consult--source-recent-file consult--source-project-recent-file
  ;; :preview-key "M-."
  :preview-key '(:debounce 0.4 any))

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
		 ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
		 ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
		 ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
		 ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
		 ;;;; 5. No project support
  ;; (setq consult-project-function nil)
  )

(use-package embark
  :ensure t
  :defer t)

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)) ;; Enable preview in Embark collect mode.

;;-----------------------------------------------------------------------------
;; Key Binders
;;-----------------------------------------------------------------------------
(use-package general
  :config
  (general-evil-setup)
  ;; Set up 'SPC' as the leader key
  (general-create-definer start/leader-keys
	:states '(normal insert visual motion emacs)
	:keymaps 'override
	:prefix "SPC"           ;; Set leader key
	:global-prefix "C-SPC") ;; Set global leader key
  
  (start/leader-keys
	"." '(find-file :wk "Find file")
	"TAB" '(comment-line :wk "Comment lines")
	"p" '(projectile-command-map :wk "Projectile command map"))
  
  (start/leader-keys
	"f" '(:ignore t :wk "Find")
	"f c" '((lambda () (interactive) (find-file "~/.config/emacs/init.el")) :wk "Edit emacs config")
	"f r" '(consult-recent-file :wk "Recent files")
	"f f" '(consult-fd :wk "Fd search for files")
	"f t" '(consult-ripgrep :wk "Ripgrep search in files")
	"f l" '(consult-line :wk "Find line")
	"f i" '(consult-imenu :wk "Imenu buffer locations"))

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
	"g" '(:ignore t :wk "Git")
	"g g" '(magit-status :wk "Magit status"))
  
  (start/leader-keys
	"h" '(:ignore t :wk "Help") ;; To get more help use C-h commands (describe variable, function, etc.)
	"h q" '(save-buffers-kill-emacs :wk "Quit Emacs and Daemon")
	"h r" '((lambda () (interactive)
			  (load-file "~/.config/emacs/init.el"))
			:wk "Reload Emacs config"))
  
  (start/leader-keys
	"t" '(:ignore t :wk "Toggle")
	"t t" '(visual-line-mode :wk "Toggle truncated lines (wrap)")
	"t l" '(display-line-numbers-mode :wk "Toggle line numbers"))
  
  (start/leader-keys
	"o" '(:ignore t :which-key "Org")
	"o i" '(org-toggle-inline-images :wk "Toggle Inline Images")

	"o t" '(:ignore t :which-key "TODO States")
	"o t t" '(org-todo :which-key "Set TODO")
	"o t d" '(lambda () (interactive) (org-todo "DOING") :which-key "Set DOING")
	"o t h" '(lambda () (interactive) (org-todo "HOLD") :which-key "Set HOLD")
	"o t D" '(lambda () (interactive) (org-todo "DONE") :which-key "Set DONE")
	"o t c" '(lambda () (interactive) (org-todo "CANCELLED") :which-key "Set CANCELLED")
	"o t m" '(lambda () (interactive) (org-todo "MAYBE") :which-key "Set MAYBE"))
  
  (start/leader-keys
	"o a" '(:ignore t :wk "Org Agenda")
	"o a c" '(org-capture :wk "Capture")
	"o a a" '(org-agenda :wk "Agenda")

	"o r" '(:ignore t :wk "Org Roam")
	"o r l" '(org-roam-buffer-toggle :wk "Toggle Buffer")
	"o r f" '(org-roam-node-find :wk "Find Node")
	"o r i" '(org-roam-node-insert :wk "Insert Node")
	"o r c" '(org-roam-capture :wk "Capture")
	"o r g" '(org-roam-graph :wk "Graph"))
  
  (start/leader-keys
	"o d" '(:ignore t :wk "Org Roam Dailies")
	"o d t" '(org-roam-dailies-capture-today :wk "Capture Today")
	"o d y" '(org-roam-dailies-capture-yesterday :wk "Capture Yesterday")
	"o d d" '(org-roam-dailies-goto-date :wk "Go-to Date")
	"o d T" '(org-roam-dailies-goto-today :wk "Go-to Today")
	"o d Y" '(org-roam-dailies-goto-yesterday :wk "Go-to Yesterday"))

  (start/leader-keys
	"-" '(lambda () (interactive) (dired default-directory) :wk "Open"))
  )
