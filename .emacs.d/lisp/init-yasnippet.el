;;; init-yasnippet.el --- settings for yasnippet -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package yasnippet
  :hook
  ((prog-mode
    markdown-mode) . yas-minor-mode)
  :config
  (yas-global-mode)

  (setq yas-indent-line 'fixed))

;;(use-package yasnippet-snippets
;;  :after yasnippet)

(hkk/ctrl-c
  ;; yasnippet
  "&"  '(:ignore t :which-key "yasnippet"))

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
