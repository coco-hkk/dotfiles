;; init-projectile.el --- settings for projectile -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "f:/github")
    (setq projectile-project-search-path '("f:/github")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

(provide 'init-projectile)
;;; init-projectile.el ends here
