;;; init-project.el --- Project Management (Projectile) -*- lexical-binding: t -*-

;;; Code:

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :config
  ;; Recommended settings for Projectile
  (setq projectile-project-search-path '("~/Documents" "~/dotfiles" "~/Documents/Projects/"))
  (setq projectile-enable-caching t))

(use-package consult-projectile
  :ensure t
  :after (consult projectile)
  :config
  (consult-projectile-mode))

(provide 'init-project)
;;; init-project.el ends here
