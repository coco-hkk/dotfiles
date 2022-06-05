;;; init-completion.el --- settings for buffer and minibuffer -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; minibuffer history save
(use-package savehist
  :config
  (savehist-mode 1))

(use-package ivy
  :hook
  (after-init . ivy-mode)
  :bind (
         ("C-s" . swiper-isearch)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-x b" . ivy-switch-buffer)
         ("M-y" . counsel-yank-pop)
         ("C-x l" . counsel-locate)

         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^

(use-package ivy-rich
  :hook (ivy-mode . ivy-rich-mode))

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

(hkk/ctrl-c
  ;; counsel
  "c"   '(:ignore t :which-key "counsel")
  "cc"  '(counsel-compile :which-key "compile")
  "cg"  '(counsel-git :which-key "git")
  "cj"  '(counsel-git-grep :which-key "git grep")
  "cL"  '(counsel-git-log :which-key "git log")
  "ck"  '(counsel-rg :which-key "rg")
  "cm"  '(counsel-linux-app :which-key "linux app")
  "cn"  '(counsel-fzf :which-key "fzf")
  "cJ"  '(counsel-file-jump :which-key "file jump")
  "cw"  '(counsel-wmctrl :which-key "wmctrl")
  "cr"  '(ivy-resume :which-key "ivy resume")
  "cb"  '(counsel-bookmark :which-key "bookmark")
  "cd"  '(counsel-descbinds :which-key "descbinds")
  "co"  '(counsel-outline :which-key "outline")
  "cF"  '(counsel-org-file :which-key "org file"))

(provide 'init-buffer)
;;; init-buffer.el ends here
