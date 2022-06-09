;;; init-completion.el --- settings for buffer and minibuffer -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; minibuffer history save
(use-package savehist
  :config
  (savehist-mode 1))

;; Buffer List manager
(use-package ibuffer
  :straight nil
  :hook ((ibuffer-mode . ibuffer-auto-mode)
         (ibuffer-mode . (lambda ()
                           (ibuffer-switch-to-saved-filter-groups "Default"))))
  :bind
  ("C-x C-b" . ibuffer)
  :custom
  (ibuffer-expert t)
  (ibuffer-movement-cycle t)
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-saved-filter-groups
   '(("Default"
      ("Emacs" (or (name . "\\*scratch\\*")
                   (name . "\\*dashboard\\*")
                   (name . "\\*compilation\\*")
                   (name . "\\*Backtrace\\*")
                   (name . "\\*Packages\\*")
                   (name . "\\*Messages\\*")
                   (name . "\\*Customize\\*")))
      ("Help" (or (name . "\\*Help\\*")
                  (name . "\\*Apropos\\*")
                  (name . "\\*info\\*")
                  (mode . Man-mode)
                  (mode . woman-mode)))
      ("Term" (or (mode . term-mode)
                  (mode . shell-mode)
                  (mode . eshell-mode)))
      ("Dict" (or (mode . fanyi-mode)
                  (mode . dictionary-mode)))
      ("Magit" (or (mode . magit-repolist-mode)
                   (mode . magit-submodule-list-mode)
                   (mode . git-rebase-mode)
                   (derived-mode . magit-section-mode)))
      ("Prog" (and (derived-mode . prog-mode)
                   (not (starred-name))))
      ("Dired" (mode . dired-mode))))))

(use-package all-the-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode))

(provide 'init-buffer)
;;; init-buffer.el ends here
