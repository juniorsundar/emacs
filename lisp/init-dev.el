;;; init-dev.el --- Development Tools, Languages, LSP -*- lexical-binding: t -*-

;;; Code:

(use-package vterm
  :ensure t)

(use-package flymake
  :ensure nil
  :defer t
  :hook ((prog-mode) . flymake-mode)
  :custom
  (flymake-margin-indicators-string
   '((error "!»" compilation-error) (warning "»" compilation-warning)
	 (note "»" compilation-info)))
  (flymake-fringe-indicator-position 'right-fringe)
  )

;;-----------------------------------------------------------------------------
;; LSP and Language Modes
;;-----------------------------------------------------------------------------
(use-package direnv
  :config
  (direnv-mode))

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

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c C-c L")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (rust-ts-mode . lsp-deferred)
         (rust-mode . lsp-deferred)
         (nix-mode . lsp-deferred)
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
  )

(use-package lsp-ui
  :ensure t
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . #'lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . #'lsp-ui-peek-find-references)))

(defun my/lsp-flymake-only ()
  "Ensure only LSP diagnostics are used in Flymake."
  (setq-local flymake-diagnostic-functions '(lsp-diagnostics-flymake-backend)))
(add-hook 'lsp-mode-hook #'my/lsp-flymake-only)

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

(provide 'init-dev)
;;; init-dev.el ends here
