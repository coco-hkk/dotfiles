;;; init-yasnippet.el --- settings for yasnippet -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package yasnippet
  :init
  (setq yas-snippet-dirs '("~/mySnippets"))
  (setq yas-indent-line (quote none))
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-global-mode))

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
