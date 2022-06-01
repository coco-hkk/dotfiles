;;; init-ui.el --- settings for theme, modeline -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; 隐藏状态栏 minor 模式
(use-package minions
  :hook (doom-modeline-mode . minions-mode)
  :custom
  (doom-modeline-minor-modes t))

;;; 图标和字体安装
(use-package all-the-icons
  :if (display-graphic-p))

;;; 主题
(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  (doom-themes-treemacs-theme "doom-atom")
  :config
  (load-theme 'doom-dracula t)
  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

;;; 状态栏配置
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom-face
  (doom-modeline ((t (:family "Segoe Print" :height 0.9))))
  (doom-modeline-inactive ((t (:family "Segoe Print" :height 0.9))))
  (doom-modeline-battery-full ((t (:inherit success :weight extra-bold))))
  :custom
  (doom-modeline-height 2)
  (doom-modeline-enable-word-count t)
  (doom-modeline-indent-info t)
  (doom-modeline-lsp t)
  (doom-modeline-buffer-file-name-style 'file-name)
  (doom-modeline-project-detection 'ffip))

;;; tab 标签配置
(use-package centaur-tabs
  :demand
  :hook
  (dashboard-mode . centaur-tabs-local-mode)
  (term-mode . centaur-tabs-local-mode)
  (calendar-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  (helpful-mode . centaur-tabs-local-mode)
  :config
  (centaur-tabs-mode t)
  (centaur-tabs-headline-match)
  :custom
  (centaur-tabs-style "slant")
  (centaur-tabs-height 32)
  (centaur-tabs-set-icons t)
  (centaur-tabs-show-navigation-buttons t)
  (centaur-tabs-set-bar 'under)
  (x-underline-at-descent-line t)
  (centaur-tabs-gray-out-icons 'buffer)
  (centaur-tabs-cycle-scope 'tabs)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward))


;;; 欢迎界面
(use-package dashboard
  :init
  ;; Format: "(icon title help action face prefix suffix)"
  (setq dashboard-navigator-buttons `(((,(if (fboundp 'all-the-icons-octicon) (all-the-icons-octicon "mark-github"      :height 1.0 :v-adjust  0.0) "★")
                                        "GitHub" "Browse" (lambda (&rest _) (browse-url homepage-url)))
                                       (,(if (fboundp 'all-the-icons-octicon) (all-the-icons-octicon "heart"            :height 1.1 :v-adjust  0.0) "♥")
                                        "Stars" "Show stars" (lambda (&rest _) (browse-url stars-url)))
                                       (,(if (fboundp 'all-the-icons-material) (all-the-icons-material "update"         :height 1.1 :v-adjust -0.2) "♺")
                                        "Update" "Update packages synchronously" (lambda (&rest _) (auto-package-update-now)) success))))

  :hook ((after-init . dashboard-setup-startup-hook)
         (dashboard-mode . (lambda ()
                             (setq-local global-hl-line-mode nil))))
  :config
  (defconst homepage-url "https://github.com/coco-hkk/dotfiles")
  (defconst stars-url (concat homepage-url "/stargazers"))
  :custom
  (dashboard-projects-backend 'projectile)                 ; 搭配 projectile 插件
  (dashboard-banner-logo-title "Nothing Is Impossible")    ; 自定义个性签名
  (dashboard-startup-banner "~/.emacs.d/img/logo.gif")     ; 自定义 logo
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-set-init-info t)
  (dashboard-set-navigator t)
  (dashboard-center-content t)
  (dashboard-items '((recents   . 8)
                     (projects  . 5)
                     (bookmarks . 5))))

;;; 快捷键自定义
(hkk/ctrl-c
  "t" '(:ignore t :which-key "centaur-tabs")
  "ts" '(centaur-tabs-counsel-switch-group :which-key "switch-group")
  "tp" '(centaur-tabs-group-by-projectile-project :which-key "group by project")
  "tg" '(centaur-tabs-group-buffer-groups :which-key "group by buffer"))

(provide 'init-ui)
;;; init-ui.el ends here
