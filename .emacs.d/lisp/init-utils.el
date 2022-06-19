;; init-utils.el --- settings for utils -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; 记录键鼠输入指令
(use-package command-log-mode
  :commands command-log-mode)

;; 帮助格式美化
(use-package helpful
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-key] . helpful-key))

;; 自动保存
(use-package super-save
  :hook (after-init . super-save-mode)
  :config
  (setq super-save-auto-save-when-idle t))

;; 自动消除尾部空白
(use-package ws-butler
  :hook ((text-mode
          prog-mode) . ws-butler-mode))

;; 用不同颜色标记多级括号
(use-package rainbow-mode
  :hook (emacs-lisp-mode-hook . rainbow-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; undo tree
(use-package undo-tree
  :hook (after-init . global-undo-tree-mode))

;; 对齐
(use-package valign
  :hook ((org-mode
         markdown-mode) . valign-mode))

(provide 'init-utils)
;;; init-utils.el ends here
