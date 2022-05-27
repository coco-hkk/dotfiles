;; init-markdown.el --- settings for markdown -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'"
  :hook (markdown-mode . (lambda ()
                           (dolist (face '((markdown-header-face-1 . 1.2)
                                           (markdown-header-face-2 . 1.1)
                                           (markdown-header-face-3 . 1.0)
                                           (markdown-header-face-4 . 1.0)
                                           (markdown-header-face-5 . 1.0)))
                             (set-face-attribute (car face) nil :weight 'normal :height (cdr face)))))
  :config
  (setq markdown-command "marked"))

;; 自动规范化 markdown
(use-package pangu-spacing
  :hook (markdown-mode . global-pangu-spacing-mode)
  :config
  (setq pangu-spacing-real-insert-separtor t))

(provide 'init-markdown)
;;; init-markdown.el ends here
