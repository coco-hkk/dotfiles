;;; init-treemacs.el --- setting for user interface, encoding, font-*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package treemacs
  :ensure t
  :defer t
  :bind
  (:map global-map
        ("M-0" . treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.2
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'detailed
          treemacs-file-event-delay                300
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t

          treemacs-indent-guide-mode               t
          ;;treemacs-indentation                     2
          ;;treemacs-indentation-string              " "
          ;;treemacs-indent-guide-style              'block

          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 300
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame

          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance

          treemacs-show-cursor                     t
          treemacs-show-hidden-files               t

          treemacs-silent-filewatch                t
          treemacs-silent-refresh                  t

          treemacs-text-scale                      nil
          treemacs-sorting                         'alphabetic-asc

          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t

          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5

          treemacs-user-mode-line-format           t
          treemacs-user-header-line-format         t

          treemacs-wide-toggle-width               30
          treemacs-width                           30
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t

          treemacs-workspace-switch-cleanup        t)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'only-when-focused)

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode t)))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))

(defhydra hydra-treemacs (
                          :color pink
                          :exit t
                          :hint nil)
  "
             ^treemacs^
--------------------------------------------
_t_: treemacs          _s_: select directory

"

  ("t" treemacs)
  ("s" treemacs-select-directory)

  ("q" nil "quit" :color red))

(hkk/leader-key
  ;; hydra keybindings
  "b" '(hydra-treemacs/body :which-key "treemacs"))

(provide 'init-treemacs)
;;; init-treemacs.el ends here
