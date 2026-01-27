;;; init-ui.el --- UI, Theme and Fonts Configurations -*- lexical-binding: t -*-

;;; Code:

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
      (set-face-font 'default "Lilex Nerd Font Mono")
      (set-face-font 'variable-pitch "IBM Plex Sans")
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
                          :family "Lilex Nerd Font Mono")
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
                    :family "Lilex Nerd Font Mono")

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

(provide 'init-ui)
;;; init-ui.el ends here
