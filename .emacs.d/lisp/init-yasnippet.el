;;; init-yasnippet.el --- settings for yasnippet -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package yasnippet
  :hook
  (prog-mode . yas-minor-mode)
  :custom
  (yas-snippet-dirs '("~/mySnippets"))
  (yas-indent-line (quote none))
  :config
  (yas-global-mode))

(hkk/ctrl-c
  ;; yasnippet
  "&"  '(:ignore t :which-key "yasnippet"))

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
