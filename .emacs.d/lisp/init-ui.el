;;; init-ui.el --- settings for theme, modeline -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; 隐藏状态栏 minor 模式
(use-package minions
  :hook (doom-modeline-mode . minions-mode)
  :config
  (setq doom-modeline-minor-modes t))

;;; 图标和字体安装
(use-package all-the-icons
  :if (display-graphic-p))

;;; 主题
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-dracula t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;;; 状态栏配置
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-enable-word-count t
        doom-modeline-minor-modes t
        doom-modeline-hud t
        doom-modeline-indent-info t
        doom-modeline-buffer-file-name-style 'file-name
        doom-modeline-project-detection 'projectile))

;;; tab 标签配置
(use-package centaur-tabs
  :hook ((dashboard-mode
          dired-mode
          term-mode
          calendar-mode
          org-agenda-mode
          helpful-mode
          dap-mode) . centaur-tabs-local-mode)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward)
  :config
  (setq centaur-tabs-style "wave"
        centaur-tabs-height 24
        centaur-tabs-set-icons t
        centaur-tabs-plain-icons t

        centaur-tabs-gray-out-icons 'buffer
        centaur-tabs-set-bar 'under
        x-underline-at-descent-line t

        ;; 不显示关闭按钮
        centaur-tabs-set-close-button nil
        centaur-tabs-adjust-buffer-order t

        centaur-tabs-show-navigation-buttons t
        centaur-tabs-show-count t
        centaur-tabs-cycle-scope 'tab)

  (centaur-tabs-mode t)
  (centaur-tabs-headline-match))

;;; 欢迎界面
(use-package dashboard
  :hook (after-init . dashboard-setup-startup-hook)
  :init
  (defconst homepage-url "https://github.com/coco-hkk/dotfiles")
  (defconst stars-url (concat homepage-url "/stargazers"))

  ;; Format: "(icon title help action face prefix suffix)"
  (setq dashboard-navigator-buttons `(((,(if (fboundp 'all-the-icons-octicon) (all-the-icons-octicon "mark-github"      :height 1.0 :v-adjust  0.0) "★")
                                        "GitHub" "Browse" (lambda (&rest _) (browse-url homepage-url)))
                                       (,(if (fboundp 'all-the-icons-octicon) (all-the-icons-octicon "heart"            :height 1.1 :v-adjust  0.0) "♥")
                                        "Stars" "Show stars" (lambda (&rest _) (browse-url stars-url)))
                                       (,(if (fboundp 'all-the-icons-material) (all-the-icons-material "update"         :height 1.1 :v-adjust -0.2) "♺")
                                        "Update" "Update packages synchronously" (lambda (&rest _) (auto-package-update-now)) success))))

  :config
  (setq dashboard-banner-logo-title "自律 · 慎独"              ; 自定义个性签名
        dashboard-startup-banner "~/.emacs.d/img/logo.png"     ; 自定义 logo
        dashboard-projects-backend 'projectile                 ; 搭配 projectile 插件
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-set-init-info t
        dashboard-set-navigator t
        dashboard-center-content t

        dashboard-items '((recents   . 5)
                          (projects  . 5)
                          (bookmarks . 5)
                          (registers . 5)
                          (agenda    . 5))))

(provide 'init-ui)
;;; init-ui.el ends here
