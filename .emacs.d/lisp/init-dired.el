;;; init-dired.el --- settings for dired -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package all-the-icons-dired
	:hook (dired-mode . all-the-icons-dired-mode))

;; Use ( to toggle dired-hide-details-mode
(use-package dired
  :straight nil
  :commands (dired dired-jump)
  :custom
  (dired-dwim-target t)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-auto-revert-buffer #'dired-directory-changed-p)
  (dired-hide-details-hide-symlink-targets nil)
  (dired-listing-switches "-AFhlv")
  (dired-omit-verbose t)
  (delete-by-moving-to-trash t)
  :config
  (add-hook 'dired-load-hook
            (lambda ()
              (interactive)
              (dired-collapse)))

  (add-hook 'dired-mode-hook
            (lambda ()
              (interactive)
              (dired-omit-mode 1)
              (dired-hide-details-mode 0)
              (hl-line-mode 1))))

;; Make dired colorful
(use-package diredfl
  :hook (dired-mode . diredfl-mode))

;; Show subtree in dired
(use-package dired-subtree
  :after dired
  :custom
  (dired-subtree-use-backgrounds nil))

(use-package dired-single
  :defer t)

(use-package dired-ranger
  :defer t)

(use-package dired-collapse
  :defer t)

(evil-collection-define-key 'normal 'dired-mode-map
  "h" 'dired-single-up-directory
  "H" 'dired-omit-mode
  "l" 'dired-single-buffer
  "y" 'dired-ranger-copy
  "X" 'dired-ranger-move
  "p" 'dired-ranger-paste)

(provide 'init-dired)
;;; init-dired.el ends here
