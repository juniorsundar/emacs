;;; init-keys.el --- General Keybindings -*- lexical-binding: t -*-

;;; Code:

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

  ;; Define the "l" (LSP) submap under the C-c leader
  (general-def
    :prefix "C-c L" ; Prefix for LSP commands
    "k" '(lsp-ui-doc-glance :which-key "LSP Documentation")
    "f" '(lsp-format-buffer :which-key "Format Buffer")
    "d" '(lsp-find-definition :which-key "LSP Definition")
    "r" '(lsp-find-references :which-key "LSP References")
    "c" '(lsp-find-declaration :which-key "LSP Declaration")
    "i" '(lsp-find-implementation :which-key "LSP Implementation")
    )

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

(provide 'init-keys)
;;; init-keys.el ends here
