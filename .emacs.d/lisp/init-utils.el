;; init-utils.el --- settings for utils -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; 记录键鼠输入指令
(use-package command-log-mode
  :defer t
  :commands command-log-mode)

;; 帮助格式美化
(use-package helpful
  :defer t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  (counsel-describe-symbol-function #'helpful-symbol)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-symbol] . counsel-describe-symbol)
  ([remap describe-key] . helpful-key))

;; 自动保存
(use-package super-save
  :hook (after-init . super-save-mode)
  :config
  (setq super-save-auto-save-when-idle t))

;; 自动消除尾部空白
(use-package ws-butler
  :defer t
  :hook ((text-mode
          prog-mode) . ws-butler-mode))

;; 用不同颜色标记多级括号
(use-package rainbow-delimiters
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Keep ~/.emacs.d clean
(use-package no-littering
  :defer 1)

(provide 'init-utils)
;;; init-utils.el ends here
