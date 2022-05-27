;;; init-keybinding.el --- settings for keybindings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; ESC Cancels All
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; 快捷键提示包
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :custom
  (which-key-idle-delay 0.5))

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

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(hkk/leader-key
  ;; hydra keybindings
  "ts" '(hydra-text-scale/body :which-key "scale text")

  ;; .emacs.d configration file
  "f"  '(:ignore t :which-key "file")
  "ff" '((lambda () (interactive) (find-file (expand-file-name "~/.emacs.d/init.el"))) :which-key "init.el")
  "fd" '((lambda () (interactive) (dired (expand-file-name "~/.emacs.d/lisp/"))) :which-key "lisp directory"))

(provide 'init-keybinding)
;;; init-keybinding.el ends here
