;; init-projectile.el --- settings for projectile -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package projectile
  :bind ("C-c p" . projectile-command-map)
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'ivy
        projectile-enable-caching t
        projectile-sort-order 'recentf
        projectile-project-search-path '("d:/Emacs/.emacs.d"
                                         "f:/github"
                                         "e:/QT"
                                         "f:/Test")))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

(hkk/ctrl-c
  ;; projectile
  "p"   '(:ignore t :which-key "projectile"))

(provide 'init-projectile)
;;; init-projectile.el ends here
