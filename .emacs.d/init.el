;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;; refer to https://github.com/daviwil/dotfiles/blob/master/Emacs.org

;;; Code:

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 100 1000 1000))

;; 从子进程一次读取最大字节数，默认 4K 修改为 1M
(setq read-process-output-max (* 1024 1024))

(push "~/.emacs.d/lisp" load-path)

;; Variables configured via the interactive 'customize' interface
(setq custom-file (locate-user-emacs-file "custom.el"))

(require 'init-package)
(require 'init-base)
(require 'init-builtin)
(require 'init-utils)
(require 'init-evil)
(require 'init-bindings)
(require 'init-completion)
(require 'init-ui)
(require 'init-buffer)
(require 'init-window)
(require 'init-dired)
(require 'init-eshell)
(require 'init-eaf)
(require 'init-org)
(require 'init-org-roam)
(require 'init-treemacs)
(require 'init-markdown)
(require 'init-yasnippet)
(require 'init-program)
(require 'init-projectile)
(require 'init-magit)
(require 'init-dict)
(require 'init-tts)
(require 'init-media)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

(load custom-file)

(provide 'init.el)
;;; init.el ends here
