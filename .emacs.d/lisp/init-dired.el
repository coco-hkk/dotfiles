;;; init-dired.el --- settings for dired -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Use ( to toggle dired-hide-details-mode
(use-package dired
  :straight (:type built-in)
  :after evil-collection
  :commands (dired dired-jump)
  :config
  ;; dired-hacks-utils
  (use-package dired-subtree)
  (use-package dired-collapse)
  (use-package dired-ranger)

  ;; dired-single
  (use-package dired-single)

  (setq dired-dwim-target t
        dired-kill-when-opening-new-dired-buffer t
        dired-hide-details-hide-symlink-targets nil
        dired-listing-switches "-AFhlv"
        dired-omit-verbose nil
        delete-by-moving-to-trash t)

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
  :hook (dired-mode . diredfl-mode))

(provide 'init-dired)
;;; init-dired.el ends here
