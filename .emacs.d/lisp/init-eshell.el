;;; init-eshell.el --- settings for eshell -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package eshell
  :hook (eshell-first-time-mode . (lambda ()
                                    ;; Save command history when commands are entered
                                    (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

                                    ;; Truncate buffer for performance
                                    (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

                                    ;; Bind some useful keys for evil-mode
                                    (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)

                                    (evil-normalize-keymaps)
                                    (setq eshell-scroll-to-bottom-on-input t)
                                    (setq eshell-hist-ignoredups t))))

(use-package eshell-git-prompt
  :after eshell
  :config
  (eshell-git-prompt-use-theme 'robbyrussell))

(use-package eshell-syntax-highlighting
  :after esh-mode
  :config
  (eshell-syntax-highlighting-global-mode +1))

(use-package eshell-toggle
  :after eshell
  :custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-run-command nil))

(hkk/leader-key
  ;; shell
  "s"   '(:ignore t :which-key "shell")

  "st"  '(eshell-toggle :which-key "toggle eshell"))

(provide 'init-eshell)
;;; init-eshell.el ends here
