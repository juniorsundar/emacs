;; Kickstart.emacs is *not* a distribution.
;; It's a template for your own configuration.

;; It is *recommeded* to configure it from the *config.org* file.
;; The goal is that you read every line, top-to-bottom, understand
;; what your configuration is doing, and modify it to suit your needs.

;; You can delete this when you're done. It's your config now. :)

;; The default is 800 kilobytes. Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(defun start/org-babel-tangle-config ()
  "Automatically tangle our Emacs.org config file when we save it. Credit to Emacs From Scratch for this one!"
  (when (string-equal (file-name-directory (buffer-file-name))
					  (expand-file-name user-emacs-directory))
	;; Dynamic scoping to the rescue
	(let ((org-confirm-babel-evaluate nil))
	  (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'start/org-babel-tangle-config)))

(require 'use-package-ensure) ;; Load use-package-always-ensure
(setq use-package-always-ensure t) ;; Always ensures that a package is installed
(setq package-archives '(("melpa" . "https://melpa.org/packages/") ;; Sets default package repositories
						 ("org" . "https://orgmode.org/elpa/")
						 ("elpa" . "https://elpa.gnu.org/packages/")
						 ("nongnu" . "https://elpa.nongnu.org/nongnu/"))) ;; For Eat Terminal

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
  (org-return-follows-link t)   ;; Sets RETURN key in org-mode to follow links
  ;; Unmap keys in 'evil-maps. If not done, org-return-follows-link will not work
  :bind (:map evil-motion-state-map
			  ("SPC" . nil)
			  ("RET" . nil)
			  ("TAB" . nil)))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-collection
  :after evil
  :config
  ;; Setting where to use evil-collection
  (setq evil-collection-mode-list '(dired ibuffer magit corfu vertico consult))
  (evil-collection-init))

										;(use-package general
										;  :config
										;  (general-evil-setup)
										;  ;; Set up 'SPC' as the leader key
										;  (general-create-definer start/leader-keys
										;	:states '(normal insert visual motion emacs)
										;	:keymaps 'override
										;	:prefix "SPC"           ;; Set leader key
										;	:global-prefix "C-SPC") ;; Set global leader key
										;
										;  (start/leader-keys
										;	"." '(find-file :wk "Find file")
										;	"TAB" '(comment-line :wk "Comment lines")
										;	"p" '(projectile-command-map :wk "Projectile command map"))
										;
										;  (start/leader-keys
										;	"f" '(:ignore t :wk "Find")
										;	"f c" '((lambda () (interactive) (find-file "~/.config/emacs/config.org")) :wk "Edit emacs config")
										;	"f r" '(consult-recent-file :wk "Recent files")
										;	"f f" '(consult-fd :wk "Fd search for files")
										;	"f t" '(consult-ripgrep :wk "Ripgrep search in files")
										;	"f l" '(consult-line :wk "Find line")
										;	"f i" '(consult-imenu :wk "Imenu buffer locations"))
										;
										;  (start/leader-keys
										;	"b" '(:ignore t :wk "Buffer Bookmarks")
										;	"b b" '(consult-buffer :wk "Switch buffer")
										;	"b k" '(kill-this-buffer :wk "Kill this buffer")
										;	"b i" '(ibuffer :wk "Ibuffer")
										;	"b n" '(next-buffer :wk "Next buffer")
										;	"b p" '(previous-buffer :wk "Previous buffer")
										;	"b r" '(revert-buffer :wk "Reload buffer")
										;	"b j" '(consult-bookmark :wk "Bookmark jump"))
										;
										;  (start/leader-keys
										;	"d" '(:ignore t :wk "Dired")
										;	"d v" '(dired :wk "Open dired")
										;	"d j" '(dired-jump :wk "Dired jump to current"))
										;
										;  (start/leader-keys
										;	"e" '(:ignore t :wk "Eglot Evaluate")
										;	"e e" '(eglot-reconnect :wk "Eglot Reconnect")
										;	"e f" '(eglot-format :wk "Eglot Format")
										;	"e l" '(consult-flymake :wk "Consult Flymake")
										;	"e b" '(eval-buffer :wk "Evaluate elisp in buffer")
										;	"e r" '(eval-region :wk "Evaluate elisp in region"))
										;
										;  (start/leader-keys
										;	"g" '(:ignore t :wk "Git")
										;	"g g" '(magit-status :wk "Magit status"))
										;
										;  (start/leader-keys
										;	"h" '(:ignore t :wk "Help") ;; To get more help use C-h commands (describe variable, function, etc.)
										;	"h q" '(save-buffers-kill-emacs :wk "Quit Emacs and Daemon")
										;	"h r" '((lambda () (interactive)
										;			  (load-file "~/.config/emacs/init.el"))
										;			:wk "Reload Emacs config"))
										;
										;  (start/leader-keys
										;	"s" '(:ignore t :wk "Show")
										;	"s e" '(eat :wk "Eat terminal"))
										;
										;  (start/leader-keys
										;	"t" '(:ignore t :wk "Toggle")
										;	"t t" '(visual-line-mode :wk "Toggle truncated lines (wrap)")
										;	"t l" '(display-line-numbers-mode :wk "Toggle line numbers"))
										;
										;  (start/leader-keys
										;	"o" '(:ignore t :which-key "Org")
										;	"o t" '(:ignore t :which-key "TODO States")
										;	"o t t" '(org-todo :which-key "Set TODO")
										;	"o t d" '(lambda () (interactive) (org-todo "DOING") :which-key "Set DOING")
										;	"o t h" '(lambda () (interactive) (org-todo "HOLD") :which-key "Set HOLD")
										;	"o t D" '(lambda () (interactive) (org-todo "DONE") :which-key "Set DONE")
										;	"o t c" '(lambda () (interactive) (org-todo "CANCELLED") :which-key "Set CANCELLED")
										;	"o t m" '(lambda () (interactive) (org-todo "MAYBE") :which-key "Set MAYBE"))
										;
										;  (start/leader-keys
										;	"o a" '(:ignore t :wk "Org Agenda")
										;	"o a c" '(org-capture :wk "Capture")
										;	"o a a" '(org-agenda :wk "Agenda")
										;
										;	"o r" '(:ignore t :wk "Org Roam")
										;	"o r l" '(org-roam-buffer-toggle :wk "Toggle Buffer")
										;	"o r f" '(org-roam-node-find :wk "Find Node")
										;	"o r i" '(org-roam-node-insert :wk "Insert Node")
										;	"o r c" '(org-roam-capture :wk "Capture")
										;	"o r g" '(org-roam-graph :wk "Graph"))
										;
										;  (start/leader-keys
										;	"o d" '(:ignore t :wk "Org Roam Dailies")
										;	"o d t" '(org-roam-dailies-capture-today :wk "Capture Today")
										;	"o d y" '(org-roam-dailies-capture-yesterday :wk "Capture Yesterday")
										;	"o d d" '(org-roam-dailies-goto-date :wk "Go-to Date")
										;	"o d T" '(org-roam-dailies-goto-today :wk "Go-to Today")
										;	"o d Y" '(org-roam-dailies-goto-yesterday :wk "Go-to Yesterday")))

(use-package emacs
  :custom
  (menu-bar-mode nil)         ;; Disable the menu bar
  (scroll-bar-mode nil)       ;; Disable the scroll bar
  (tool-bar-mode nil)         ;; Disable the tool bar
  ;;(inhibit-startup-screen t)  ;; Disable welcome screen

  (delete-selection-mode t)   ;; Select text and delete it by typing.
  (electric-indent-mode nil)  ;; Turn off the weird indenting that Emacs does by default.
  (electric-pair-mode t)      ;; Turns on automatic parens pairing

  (blink-cursor-mode nil)     ;; Don't blink cursor
  (global-auto-revert-mode t) ;; Automatically reload file and show changes if the file has changed

  (dired-kill-when-opening-new-dired-buffer t) ;; Dired don't create new buffer
  (recentf-mode t) ;; Enable recent file mode

  ;;(global-visual-line-mode t)           ;; Enable truncated lines
  (display-line-numbers-type 'relative) ;; Relative line numbers
  (global-display-line-numbers-mode t)  ;; Display line numbers

  (mouse-wheel-progressive-speed nil) ;; Disable progressive speed when scrolling
  (scroll-conservatively 10) ;; Smooth scrolling
  ;;(scroll-margin 8)

  (tab-width 4)

  (make-backup-files nil) ;; Stop creating ~ backup files
  (auto-save-default nil) ;; Stop creating # auto save files
  :hook
  (prog-mode . (lambda () (hs-minor-mode t))) ;; Enable folding hide/show globally
  :config
  ;; Move customization variables to a separate file and load it, avoid filling up init.el with unnecessary variables
  (setq custom-file (locate-user-emacs-file "custom-vars.el"))
  (load custom-file 'noerror 'nomessage)
  :bind
  ([escape] . keyboard-escape-quit) ;; Makes Escape quit prompts (Minibuffer Escape)
  ("C-+" . text-scale-increase)
  ("C--" . text-scale-decrease)
  ("<C-wheel-up>" . text-scale-increase)
  ("<C-wheel-down>" . text-scale-decrease)
  ;; Fix general.el leader key not working instantly in messages buffer with evil mode
  )

(add-to-list 'exec-path "/usr/local/bin/go/bin")	
(add-to-list 'exec-path "/usr/local/go/bin")	
(add-to-list 'exec-path "/usr/local/bin")	
(add-to-list 'exec-path "~/.local/bin")	
(add-to-list 'exec-path "~/go/bin")	
(add-to-list 'exec-path "/usr/bin")	
(add-to-list 'exec-path "~/anaconda3/bin")	
(add-to-list 'exec-path "~/.nvm/versions/node/v20.15.0/bin/")

										;(use-package dashboard
										;  :ensure t
										;  :config
										;  (dashboard-setup-startup-hook))

(defvar my-config-dir (expand-file-name "lisp" user-emacs-directory)
  "Directory containing my configuration files.")

;; Function to load a configuration file
(defun load-config-file (file)
  (load (expand-file-name file my-config-dir)))

(load-config-file "cyberdream-theme.el")
(load-theme 'cyberdream :no-confirm) ;; We need to add t to trust this package
(setq cyberdream-flavor 'mocha)
(cyberdream-reload)

(add-to-list 'default-frame-alist '(alpha-background . 100)) ;; For all new frames henceforth

;; Set Nerd Font for symbols
(let ((font-spec (font-spec :family "Symbols Nerd Font Mono" :size 18)))
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

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 25)     ;; Sets modeline height
  (doom-modeline-bar-width 5)   ;; Sets right bar width
  (doom-modeline-persp-name t)  ;; Adds perspective name to modeline
  (doom-modeline-persp-icon t)) ;; Adds folder icon next to persp name

(use-package projectile
  :init
  (projectile-mode)
  :custom
  (projectile-run-use-comint-mode t) ;; Interactive run dialog when running projects inside emacs (like giving input)
  (projectile-switch-project-action #'projectile-dired) ;; Open dired when switching to a project
  (projectile-project-search-path '("~/projects/" "~/work/"))) ;; . 1 means only search the first subdirectory level for projects
;; Use Bookmarks for smaller, not standard projects

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (go-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui 
  :init
  (setq lsp-ui-doc-show-with-cursor t)
  :commands lsp-ui-mode
)

(add-hook 'go-mode-hook #'lsp-deferred)
;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(use-package yasnippet-snippets
  :hook (prog-mode . yas-minor-mode))

(use-package org
  :ensure t
  :hook
  ;; (org-mode . org-indent-mode) ;; Indent text
  (org-mode . visual-line-mode)
  :custom
  (org-return-follows-link t))

;; Ensure inline images are displayed when opening an Org file
(setq org-startup-with-inline-images t)
;; Function to display images
(defun display-inline-images ()
  "Display inline images in the buffer."
  (org-display-inline-images))
;; Add the display function to the Org mode hook
(add-hook 'org-mode-hook 'display-inline-images)

;; Set Org directory
(setq org-directory "~/Dropbox/neorg/org/")

;; Recursive function to find all .org files in a directory
(defun my/org-agenda-files-recursive (directory)
  "Recursively find all .org files in DIRECTORY."
  (let ((org-file-list '()))
	(dolist (file (directory-files-recursively directory "\\.org$"))
	  (setq org-file-list (append org-file-list (list file))))
	org-file-list))

(setq org-agenda-files (my/org-agenda-files-recursive "~/Dropbox/neorg/org/org-roam/"))

;; Customize agenda prefix format
(setq org-agenda-prefix-format
	  '((agenda . " %i %?-12t% s")  ; remove file name
		(todo . " %i ")
		(tags . " %i ")
		(search . " %i ")))

;; Define TODO keywords and their faces
(setq org-todo-keywords
	  '((sequence "TODO(t)" "DOING(d)" "HOLD(h)" "|" "DONE(D)" "CANCELLED(c)" "MAYBE(m)")))

(setq org-todo-keyword-faces
	  '(("DOING" . "yellow")
		("HOLD" . "magenta")
		("CANCELLED" . "red")
		("MAYBE" . "orange")))

;; Set default notes file
(setq org-default-notes-file (concat org-directory "/inbox.org"))

;; Define capture templates
(setq org-capture-templates
	  '(("t" "Blank Todo [inbox]" entry
		 (file+headline "~/Dropbox/neorg/org/inbox.org" "Tasks")
		 "* TODO %i%?")
		("w" "Work Todo [work]" entry
		 (file+headline "~/Dropbox/neorg/org/work.org" "Work")
		 "* TODO %i%?")
		("p" "Personal Todo [personal]" entry
		 (file+headline "~/Dropbox/neorg/org/personal.org" "Personal")
		 "* TODO %i%?")))

;; Conceal emphasis markers for bold and italic text
(setq org-hide-emphasis-markers t)

;; Customize the appearance of inline code #45475a #c6d0f5
(custom-set-faces
 '(org-code ((t (:background "#1e2124" :foreground "#ffffff" :family "JetBrainsMono NFM")))))

(custom-set-faces
 '(org-emphasis ((t (:underline nil :foreground nil :background nil))))
 '(org-bold ((t (:weight bold :foreground "#f2cdcd" :background nil))))
 '(org-italic ((t (:slant italic :underline nil :foreground "#c6d0f5" :background nil))))
 '(org-underline ((t (:underline t :foreground nil :background nil)))))

(defface org-block-note
  '((t (:background "#F9E2AF" :foreground "#000000")))
  "Face for Note blocks in Org mode.")

(defface org-block-warn
  '((t (:background "#F38BA8" :foreground "#000000")))
  "Face for Warn blocks in Org mode.")

(defface org-block-important
  '((t (:background "#A6E3A1" :foreground "#000000")))
  "Face for Important blocks in Org mode.")

(defun my/org-add-custom-block-faces ()
  (font-lock-add-keywords nil
						  '(("\\(#\\+begin_note\\|#\\+end_note\\)" 1 'org-block-note prepend)
							("\\(#\\+begin_warn\\|#\\+end_warn\\)" 1 'org-block-warn prepend)
							("\\(#\\+begin_important\\|#\\+end_important\\)" 1 'org-block-important prepend)
							("\\(#\\+begin_note\\)[ \t]*\\(.*\\)"
							 (1 'org-block-note prepend)
							 (2 'org-block-note prepend))
							("\\(#\\+begin_warn\\)[ \t]*\\(.*\\)"
							 (1 'org-block-warn prepend)
							 (2 'org-block-warn prepend))
							("\\(#\\+begin_important\\)[ \t]*\\(.*\\)"
							 (1 'org-block-important prepend)
							 (2 'org-block-important prepend)))
						  t)
  (font-lock-add-keywords nil
						  '(("\\(#\\+begin_note\\)\\(.\\|\n\\)*?\\(#\\+end_note\\)"
							 (0 'org-block-note prepend))
							("\\(#\\+begin_warn\\)\\(.\\|\n\\)*?\\(#\\+end_warn\\)"
							 (0 'org-block-warn prepend))
							("\\(#\\+begin_important\\)\\(.\\|\n\\)*?\\(#\\+end_important\\)"
							 (0 'org-block-important prepend)))
						  t)
  (font-lock-flush))

(add-hook 'org-mode-hook 'my/org-add-custom-block-faces)

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/Dropbox/neorg/org/org-roam/"))
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
  :hook (org-mode . org-superstar-mode))

(use-package org-tempo
  :ensure nil
  :after org)

(use-package eat
  :hook ('eshell-load-hook #'eat-eshell-mode))

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

(use-package magit
  :commands magit-status)

(use-package diff-hl
  :hook ((dired-mode         . diff-hl-dired-mode-unless-remote)
		 (magit-pre-refresh  . diff-hl-magit-pre-refresh)
		 (magit-post-refresh . diff-hl-magit-post-refresh))
  :init
  (global-diff-hl-mode))

(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-prefix 2)          ;; Minimum length of prefix for auto completion.
  (corfu-popupinfo-mode t)       ;; Enable popup information
  (corfu-popupinfo-delay 0.5)    ;; Lower popupinfo delay to 0.5 seconds from 2 seconds
  (corfu-separator ?\s)          ;; Orderless field separator, Use M-SPC to enter separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin
  (completion-ignore-case t)
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)
  (corfu-preview-current nil) ;; Don't insert completion without confirmation
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

  (add-to-list 'completion-at-point-functions #'cape-dabbrev) ;; Complete word from current buffers
  (add-to-list 'completion-at-point-functions #'cape-dict) ;; Dictionary completion
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
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package vertico
  :init
  (vertico-mode))

(savehist-mode) ;; Enables save history mode

(use-package marginalia
  :after vertico
  :init
  (marginalia-mode))

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
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))

  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  ;; (consult-customize
  ;; consult-theme :preview-key '(:debounce 0.2 any)
  ;; consult-ripgrep consult-git-grep consult-grep
  ;; consult-bookmark consult-recent-file consult-xref
  ;; consult--source-bookmark consult--source-file-register
  ;; consult--source-recent-file consult--source-project-recent-file
  ;; :preview-key "M-."
  ;; :preview-key '(:debounce 0.4 any))

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
		 ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
		 ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
		 ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
		 ;;;; 4. projectile.el (projectile-project-root)
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))
		 ;;;; 5. No project support
  ;; (setq consult-project-function nil)
  )

(use-package diminish)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

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

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
;; Increase the amount of data which Emacs reads from the process
(setq read-process-output-max (* 1024 1024)) ;; 1mb
