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

;; --- Package Management (Elpaca) ---
;; We must set up the package manager before `init.el` tries to use it.
;; NOTE: I removed your old `use-package-ensure` and `package-archives` lines,
;; as Elpaca and `elpaca-use-package` make them redundant.
(require 'use-package-ensure)
(setq use-package-always-ensure t) 
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(setq package-enable-at-startup nil)
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                             :ref nil :depth 1 :inherit ignore
                             :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                             :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

;;; early-init.el ends here
