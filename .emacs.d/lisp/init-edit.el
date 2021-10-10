;;; init-edit.el --- settings for editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; 注释
(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

;; auto save
(use-package super-save
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
