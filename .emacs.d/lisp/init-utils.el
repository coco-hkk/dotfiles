;; init-utils.el --- settings for utils -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; 记录键鼠输入指令
(use-package command-log-mode
  :commands command-log-mode)

;; 帮助格式美化
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; 自动保存
(use-package super-save
  :diminish super-save-mode
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t))

;; 自动消除尾部空白
(use-package ws-butler
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))

;; 用不同颜色标记多级括号
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Keep ~/.emacs.d clean
(use-package no-littering)

(provide 'init-utils)
;;; init-utils.el ends here
