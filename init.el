;;; init.el --- My Emacs Config -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Author: Junior Sundar
;; Version: 0.1.0
;; Package-Requires: ((Emacs "30.0"))
;;
;;; Code:

;; Add lisp/ directory to load-path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Load modules
(require 'init-core)
(require 'init-ui)
(require 'init-editor)
(require 'init-completion)
(require 'init-dev)
(require 'init-project)
(require 'init-org)
(require 'init-keys)

;;; init.el ends here
