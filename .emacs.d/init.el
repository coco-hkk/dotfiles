;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;; refer to https://github.com/daviwil/dotfiles/blob/master/Emacs.org

;;; Code:

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(push "~/.emacs.d/lisp" load-path)

(setq custom-file (locate-user-emacs-file "custom.el"))

(require 'init-package)
(require 'init-base)
(require 'init-utils)
(require 'init-evil)
(require 'init-keybinding)
(require 'init-ui)
(require 'init-buffer)
(require 'init-edit)
(require 'init-window)
(require 'init-dired)
(require 'init-eshell)
(require 'init-media)
(require 'init-org)
(require 'init-lsp)
(require 'init-git)
(require 'init-yasnippet)
(require 'init-markdown)
(require 'init-projectile)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

;; Variables configured via the interactive 'customize' interface
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init.el)
;;; init.el ends here
