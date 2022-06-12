;; init-projectile.el --- settings for projectile -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package projectile
  :defer t
  :config
  (projectile-mode)
  :custom
  (projectile-completion-system 'ivy)
  (projectile-project-search-path '("f:/github" "d:/Emacs/.emacs.d" "f:/Test"))
  (projectile-sort-order 'recentf)
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

(hkk/ctrl-c
  ;; projectile
  "p"   '(:ignore t :which-key "projectile"))

(provide 'init-projectile)
;;; init-projectile.el ends here
