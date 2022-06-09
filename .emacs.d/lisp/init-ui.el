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
  :config
  (load-theme 'doom-dracula t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;;; 状态栏配置
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-enable-word-count t)
  (doom-modeline-minor-modes t)
  (doom-modeline-hud t)
  (doom-modeline-indent-info t)
  (doom-modeline-buffer-file-name-style 'file-name)
  (doom-modeline-project-detection 'projectile))

;;; tab 标签配置
(use-package centaur-tabs
  :demand
  :hook
  ((dashboard-mode
    dired-mode
    term-mode
    calendar-mode
    org-agenda-mode
    helpful-mode
    dap-mode) . centaur-tabs-local-mode)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward)
  :custom
  (centaur-tabs-style "wave")
  (centaur-tabs-height 24)
  (centaur-tabs-set-icons t)
  (centaur-tabs-plain-icons t)

  (centaur-tabs-gray-out-icons 'buffer)
  (centaur-tabs-set-bar 'under)
  (x-underline-at-descent-line t)

  (centaur-tabs-set-close-button nil)           ;; 不显示关闭按钮
  (centaur-tabs-adjust-buffer-order t)

  (centaur-tabs-show-navigation-buttons t)
  (centaur-tabs-show-count t)
  (centaur-tabs-cycle-scope 'tab)
  :config
  (centaur-tabs-mode t)
  (centaur-tabs-headline-match)

  (defun centaur-tabs-buffer-groups ()
    (list
     (cond
      ((or (string-equal "*" (substring (buffer-name) 0 1))
           (memq major-mode '(magit-process-mode
                              magit-status-mode
                              magit-diff-mode
                              magit-log-mode
                              magit-file-mode
                              magit-blob-mode
                              magit-blame-mode
                              )))
       "Emacs")
      ((derived-mode-p 'prog-mode)
       "Editing")
      ((derived-mode-p 'dired-mode)
       "Dired")
      ((memq major-mode '(helpful-mode
                          help-mode))
       "Help")
      ((memq major-mode '(org-mode
                          org-agenda-clockreport-mode
                          org-src-mode
                          org-agenda-mode
                          org-beamer-mode
                          org-indent-mode
                          org-bullets-mode
                          org-cdlatex-mode
                          org-agenda-log-mode
                          diary-mode))
       "OrgMode")
      (t
       (centaur-tabs-get-group-name (current-buffer)))))))

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
  (dashboard-banner-logo-title "Nothing Is Impossible")    ; 自定义个性签名
  (dashboard-startup-banner "~/.emacs.d/img/logo.gif")     ; 自定义 logo
  (dashboard-projects-backend 'projectile)                 ; 搭配 projectile 插件
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-set-init-info t)
  (dashboard-set-navigator t)
  (dashboard-center-content t)
  (dashboard-items '((recents   . 8)
                     (projects  . 5)
                     (bookmarks . 5))))

(defhydra hydra-ui (:color pink
                           :exit t
                           :hint nil)
  "
  ^Centaur^
-----------------------------------------------------------------
_cs_: switch group
_cp_: group by project
_cg_: group by buffer

"
  ("cs" centaur-tabs-counsel-switch-group)
  ("cp" centaur-tabs-group-by-projectile-project)
  ("cg" centaur-tabs-group-buffer-groups)

  ("q" nil "quit" :color pink))

(hkk/leader-key
  ;; hydra keybindings
  "u" '(hydra-ui/body :which-key "UI"))

(provide 'init-ui)
;;; init-ui.el ends here
