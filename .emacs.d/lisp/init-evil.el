;;; init-evil.el --- settings for evil -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package evil
  :custom
  (evil-want-integration t)
  (evil-want-keybinding nil)
  (org-export-with-broken-links t)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

  ;; eaf
  (evil-set-initial-state 'eaf-mode 'emacs)
  (setq evil-buffer-regexps '(("^\\*Outline:.*" . 'emacs))))

(use-package evil-collection
  :after (evil general)
  :config
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

;; 注释
(use-package evil-nerd-commenter
  :after evil
  :bind
  ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package sis
  :after evil
  ;; :hook
  ;; enable the /follow context/ and /inline region/ mode for specific buffers
  ;; (((text-mode prog-mode) . sis-context-mode)
  ;;  ((text-mode prog-mode) . sis-inline-mode))

  :config
  (sis-ism-lazyman-config "1033" "2052" 'im-select)

  ;; enable the /cursor color/ mode
  (sis-global-cursor-color-mode t)
  ;; enable the /respect/ mode
  (sis-global-respect-mode t)
  ;; enable the /context/ mode for all buffers
  (sis-global-context-mode t)
  ;; enable the /inline english/ mode for all buffers
  (sis-global-inline-mode t))

(provide 'init-evil)
;;; init-evil.el ends here
