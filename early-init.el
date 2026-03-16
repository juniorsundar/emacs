;;; early-init.el --- Run before package-init -*- lexical-binding: t -*-

;;; Code:

;; --- Performance Tweaks ---
;; Set GC threshold high during startup
(setq gc-cons-threshold #x40000000)
(setq read-process-output-max (* 1024 1024 4))

;; --- Disable UI Elements BEFORE They Load ---
;; This is a major performance boost
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-hl-line-mode 1)
(indent-tabs-mode -1)
(modify-coding-system-alist 'file "" 'utf-8)

;; Minibuffer Improvements
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; --- Basic Emacs Customization ---
;; All these are native `setq`s from your `(use-package emacs)` block.
;; They are safe to set here.
(setq frame-resize-pixelwise t)
(setq auto-save-default nil)
(setq create-lockfiles nil)
(setq delete-by-moving-to-trash t)
(setq delete-selection-mode t)
(setq display-line-numbers-type 'relative)
(setq global-auto-revert-non-file-buffers t)
(setq history-length 25)
(setq inhibit-startup-screen t)
(setq initial-scratch-message "")
(setq ispell-program-name "aspell")
(setq ispell-dictionary "en_US")
(setq make-backup-files nil)
(setq line-number-mode nil)
(setq pixel-scroll-precision-mode t)
(setq pixel-scroll-precision-use-momentum nil)
(setq ring-bell-function 'ignore)
(setq split-width-threshold 300)
(setq switch-to-buffer-obey-display-actions t)
(setq tab-always-indent 'complete)
(setq tab-width 4)
(setq truncate-lines t)
(setq use-dialog-box nil)
(setq use-short-answers t)
(setq warning-minimum-level :emergency)
(setq mouse-wheel-progressive-speed nil)
(setq scroll-conservatively 10)
(setq enable-recursive-minibuffers t)
(setq electric-pair-mode t)
(setq blink-cursor-mode nil)
(setq xterm-mouse-mode 1)
(setq recentf-mode t)
(setq savehist-mode t)
(setq save-place-mode t)
(setq winner-mode t)
(setq file-name-shadow-mode t)
(setq treesit-font-lock-level 4)

;; --- Path Setup ---
;; Set paths early so Emacs can find 'git' for elpaca
(dolist (path '("/usr/local/go/bin"
                "/usr/local/bin"
                "~/.local/bin"
                "/usr/bin"))
  (add-to-list 'exec-path (expand-file-name path)))

;; --- Package Management (package.el) ---
;; Enable package.el and set up archives
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;;; early-init.el ends here
