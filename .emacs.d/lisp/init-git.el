;; init-git.el --- settings for magit -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(advice-add #'magit-version :override #'ignore)

(provide 'init-git)
;;; init-git.el ends here
