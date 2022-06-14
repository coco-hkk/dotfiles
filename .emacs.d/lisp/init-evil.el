;;; init-evil.el --- settings for evil -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package evil
  :hook (after-init . evil-mode)
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil)
  :config
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

  ;; eaf
  (evil-set-initial-state 'eaf-mode 'emacs)
  (setq evil-buffer-regexps '(("^\\*Outline:.*" . 'emacs))))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :hook (evil-mode . global-evil-surround-mode))

;; 注释
(use-package evil-nerd-commenter
  :bind
  ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package sis
  :after evil
  :config
  (sis-ism-lazyman-config "1033" "2052" 'im-select)

  (sis-global-cursor-color-mode t)
  (sis-global-respect-mode t)
  (sis-global-context-mode t)
  (sis-global-inline-mode t))

(provide 'init-evil)
;;; init-evil.el ends here
