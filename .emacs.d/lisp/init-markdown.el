;; init-markdown.el --- settings for markdown -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package markdown-mode
  :mode "\\.md\\'"
  :config
  (setq markdown-command "marked")
  (defun hkk/set-markdown-header-font-sizes ()
    (dolist (face '((markdown-header-face-1 . 1.2)
                    (markdown-header-face-2 . 1.1)
                    (markdown-header-face-3 . 1.0)
                    (markdown-header-face-4 . 1.0)
                    (markdown-header-face-5 . 1.0)))
      (set-face-attribute (car face) nil :weight 'normal :height (cdr face))))

  (defun hkk/markdown-mode-hook ()
    (hkk/set-markdown-header-font-sizes))

  (add-hook 'markdown-mode-hook 'hkk/markdown-mode-hook))

(use-package pangu-spacing
  :hook (markdown-mode . global-pangu-spacing-mode)
  :config
  (setq pangu-spacing-real-insert-separtor t))

(provide 'init-markdown)
;;; init-markdown.el ends here
