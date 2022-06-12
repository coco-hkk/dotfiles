;;; init-dired.el --- settings for dired -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Use ( to toggle dired-hide-details-mode
(use-package dired
  :straight (:type built-in)
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
              (hl-line-mode 1)))
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "H" 'dired-omit-mode
    "l" 'dired-single-buffer
    "y" 'dired-ranger-copy
    "X" 'dired-ranger-move
    "p" 'dired-ranger-paste))

;; Make dired colorful
(use-package diredfl
  :after dired
  :hook (dired-mode . diredfl-mode))

;; Show subtree in dired
(use-package dired-hacks-utils
  :straight (dired-hacks-utils :files ("*.el"))
  :after dired
  :config
  (require 'dired-subtree)
  (require 'dired-collapse)
  (require 'dired-ranger))

(use-package dired-single
  :after dired)

(provide 'init-dired)
;;; init-dired.el ends here
