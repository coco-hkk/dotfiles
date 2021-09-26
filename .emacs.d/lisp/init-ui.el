;;; init-ui.el --- settings for theme, modeline -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package doom-themes
  :init
  (load-theme 'doom-dracula t)
  :config
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (setq doom-themes-treemacs-theme "doom-atom")
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(use-package all-the-icons
  :when (display-graphic-p)
  :demand t)

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-github nil)
  (doom-modeline-mu4e nil)
  (doom-modeline-persp-name nil)
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-irc nil)
  (doom-modeline-lsp t)
  (doom-modeline-minor-modes t)
  (doom-modeline-buffer-file-name-style 'file))

(use-package minions
  :hook (doom-modeline-mode . minions-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package paren
  :defer 1
  :config
  (set-face-attribute 'show-paren-match-expression nil :background "#363e4a")
  (show-paren-mode 1))

(use-package dashboard
  :init
  ;; Format: "(icon title help action face prefix suffix)"
  (setq dashboard-navigator-buttons `(((,(if (fboundp 'all-the-icons-octicon) (all-the-icons-octicon "mark-github"      :height 1.0 :v-adjust  0.0) "★")
                                        "GitHub" "Browse" (lambda (&rest _) (browse-url homepage-url)))
                                       (,(if (fboundp 'all-the-icons-octicon) (all-the-icons-octicon "heart"            :height 1.1 :v-adjust  0.0) "♥")
                                        "Stars" "Show stars" (lambda (&rest _) (browse-url stars-url)))
                                       (,(if (fboundp 'all-the-icons-material) (all-the-icons-material "report_problem" :height 1.1 :v-adjust -0.2) "⚑")
                                        "Issue" "Report issue" (lambda (&rest _) (browse-url issue-url)) warning)
                                       (,(if (fboundp 'all-the-icons-material) (all-the-icons-material "update"         :height 1.1 :v-adjust -0.2) "♺")
                                        "Update" "Update packages synchronously" (lambda (&rest _) (auto-package-update-now)) success))))

  :hook ((after-init . dashboard-setup-startup-hook)
         (dashboard-mode . (lambda ()
                             (setq-local global-hl-line-mode nil))))
  :config
  (defconst homepage-url "https://github.com/coco-hkk/dotfiles")
  (defconst stars-url (concat homepage-url "/stargazers"))
  (defconst issue-url (concat homepage-url "/issues/new"))
  :custom
  (dashboard-startup-banner 'logo)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-set-init-info t)
  (dashboard-set-navigator t)
  (dashboard-items '((recents   . 10)
                     (projects  . 5)
                     (bookmarks . 5))))

(es/leader-key-def
  "t"  '(:ignore t :which-key "toggles")
  "tw" 'whitespace-mode
  "tr" '(treemacs :which-key "treemacs")
  "tt" '(counsel-load-theme :which-key "choose theme"))

(provide 'init-ui)
;;; init-ui.el ends here
