;;; init-bind.el --- settings for keybindings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; ESC Cancels All
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; 快捷键提示包
(use-package which-key
  :hook (after-init . which-key-mode)
  :custom
  (which-key-idle-delay 0.4))

(use-package general
  :config
  (general-evil-setup t)

  (general-create-definer hkk/leader-key
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (general-create-definer hkk/ctrl-c
    :prefix "C-c"))

(use-package hydra
  :after general)

(use-package major-mode-hydra
  :straight (major-mode-hydra :files ("*.el"))
  :after hydra)

(use-package hydra-posframe
  :straight (hydra-posframe :type git
                            :host github
                            :repo "Ladicle/hydra-posframe")
  :after hydra
  :init
  (setq hydra-posframe-border-width 3
        hydra-posframe-parameters '((left-fringe . 8)
                                    (right-fringe . 8)))
  :config
  (hydra-posframe-mode))

(provide 'init-bind)
;;; init-bind.el ends here
