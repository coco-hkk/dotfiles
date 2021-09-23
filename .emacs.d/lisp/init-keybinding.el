;;; init-keybinding.el --- settings for keybindings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; ESC Cancels All
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package general
  :config
  (general-evil-setup t)

  (general-create-definer es/leader-key-def
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (general-create-definer es/ctrl-c-keys
    :prefix "C-c")

  (es/leader-key-def
    "f"  '(:ignore t :which-key "file")
    "fi" '((lambda () (interactive) (find-file (expand-file-name "~/.emacs.d/init.el"))) :which-key "init file")))

(use-package hydra
  :after general)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(es/leader-key-def
  "ts" '(hydra-text-scale/body :which-key "scale text"))

(provide 'init-keybinding)
;;; init-keybinding.el ends here
