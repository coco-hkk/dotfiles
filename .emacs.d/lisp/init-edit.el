;;; init-edit.el --- settings for editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; tab width setting
(setq-default tab-width 2)
(setq-default evil-shift-width tab-width)

;; 空格代替 tab
(setq-default indent-tabs-mode nil)

;; 注释
(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

;; auto save
(use-package super-save
  :defer 1
  :diminish super-save-mode
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t))

;; delete whitespace
(use-package ws-butler
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))

(provide 'init-edit)
;;; init-edit.el ends here
