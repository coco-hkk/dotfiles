;;; init-builtin.el --- settings for window -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; 记录上次关闭文件时 cursor 停留位置
(use-package saveplace
  :straight (:type built-in)
  :hook (after-init . save-place-mode))

;;; recentf
(use-package recentf
  :straight (:type built-in)
  :hook (after-init . recentf-mode))

;; minibuffer history save
(use-package savehist
  :straight (:type built-in)
  :hook (after-init . savehist-mode))

;;; 高亮当前行
(use-package hl-line
  :straight (:type built-in)
  :hook (after-init . global-hl-line-mode))

;;; 隐藏、显示结构化数据，如 {} 内容，注释内容
(use-package hideshow
  :straight (:type built-in)
  :hook (prog-mode . hs-minor-mode)
  :custom
  (hs-special-modes-alist
   (mapcar 'purecopy
           '((c-mode "{" "}" "/[*/]" nil nil)
             (c++-mode "{" "}" "/[*/]" nil nil)))))

;;; 处理文件中特别长的行，防止界面卡死
(use-package so-long
  :straight (:type built-in)
  :hook (after-init . global-so-long-mode))

;;; Emacs 打开的文件若在硬盘中被修改，需要自动更新 buffer
(use-package autorevert
  :straight (:type built-in)
  :hook
  (after-init . global-auto-revert-mode)
  :config
  (setq global-auto-revert-non-file-buffers t))

;;; 选中文本后，直接输入就可以，省去了删除操作
(use-package delsel
  :straight (:type built-in)
  :hook (after-init . delete-selection-mode))

;;; parenthesis 高亮显示配对的( ) [ ] { } 括号
(use-package paren
  :straight (:type built-in)
  :hook (after-init . show-paren-mode)
  :custom
  (show-paren-style 'mixed)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t))

;;; ERC
(use-package erc
  :defer t
  :straight (:type built-in)
  :defines erc-autojoin-channels-alist
  :config
  (setq erc-rename-buffers t
        erc-interpret-mirc-color t
        erc-lurker-hide-list '("JOIN" "PART" "QUIT")
        erc-autojoin-channels-alist '(("freenode.net" "#emacs"))))

;; Process
(use-package proced
  :defer t
  :straight (:type built-in)
  :hook (proced-mode . (lambda () (proced-toggle-auto-update 1)))
  :config
  (setq-default proced-format 'verbose)
  (setq proced-auto-update-flag t
        proced-auto-update-interval 3))

(provide 'init-builtin)
;;; init-builtin.el ends here
