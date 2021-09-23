;;; init-package.el --- package management -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; gpg 签名设置
;;(setq package-check-signature nil)    ; 禁掉签名
(setq package-gnupghome-dir "/d/emacs-27.2/.emacs.d/elpa/gnupg")

;; Initialize package sources
(require 'package)

(setq package-archives
  '(("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
    ("org"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
    ("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; hides minor modes from the modelines
(use-package diminish)

;; auto udpate packages
(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

(provide 'init-package)
;;; init-package.el ends here
