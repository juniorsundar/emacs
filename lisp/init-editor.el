;;; init-editor.el --- Editing Configurations (Evil/Meow) -*- lexical-binding: t -*-

;;; Code:

(use-package smerge-mode
  :ensure nil
  :defer t
  :bind (:map smerge-mode-map
              ("C-c s u" . smerge-keep-upper)  ;; Keep the changes from the upper version.
              ("C-c s l" . smerge-keep-lower)  ;; Keep the changes from the lower version.
              ("C-c s n" . smerge-next)        ;; Move to the next conflict.
              ("C-c s p" . smerge-previous)))  ;; Move to the previous conflict.

;;-----------------------------------------------------------------------------
;; Evil (Avy is often used with evil/meow)
;;-----------------------------------------------------------------------------
(use-package avy
  :ensure t)

(use-package wgrep
  :ensure t)

;;-----------------------------------------------------------------------------
;; Meow
;;-----------------------------------------------------------------------------
(use-package meow
  :ensure t
  :config
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
     '("<escape>" . ignore)))
  (require 'meow)
  (meow-setup)
  (meow-global-mode 1)
  )

(use-package multiple-cursors
  :ensure t
  :config
  (require 'multiple-cursors)
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

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

(provide 'init-editor)
;;; init-editor.el ends here
