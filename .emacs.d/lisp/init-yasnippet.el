;;; init-yasnippet.el --- settings for yasnippet -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package yasnippet
  :hook
  (prog-mode . yas-minor-mode)
  :custom
  (yas-indent-line 'fixed)
  :config
  (yas-global-mode))

;;(use-package yasnippet-snippets
;;  :after yasnippet)

(hkk/ctrl-c
  ;; yasnippet
  "&"  '(:ignore t :which-key "yasnippet"))

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
