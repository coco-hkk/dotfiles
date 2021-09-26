;;; init-completion.el --- settings for buffer and minibuffer -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; minibuffer history save
(use-package savehist
  :config
  (savehist-mode 1))

(use-package ivy
  :diminish ivy-mode
  :hook (after-init . ivy-mode)
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
  (ibuffer-movement-cycle nil)
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
      ("News" (or (name . "\\*newsticker\\*")))
      ("Help" (or (name . "\\*Help\\*")
                  (name . "\\*Apropos\\*")
                  (name . "\\*info\\*")
                  (mode . Man-mode)
                  (mode . woman-mode)))
      ("Repl" (or (mode . gnuplot-comint-mode)
                  (mode . inferior-emacs-lisp-mode)
                  (mode . inferior-python-mode)))
      ("Term" (or (mode . term-mode)
                  (mode . shell-mode)
                  (mode . eshell-mode)))
      ("Mail" (or (mode . mail-mode)
                  (mode . message-mode)
                  (derived-mode . gnus-mode)))
      ("Conf" (or (mode . yaml-mode)
                  (mode . conf-mode)))
      ("Dict" (or (mode . fanyi-mode)
                  (mode . dictionary-mode)))
      ("Text" (and (derived-mode . text-mode)
                   (not (starred-name))))
      ("Magit" (or (mode . magit-repolist-mode)
                   (mode . magit-submodule-list-mode)
                   (mode . git-rebase-mode)
                   (derived-mode . magit-section-mode)))
      ("VC" (or (mode . diff-mode)
                (derived-mode . log-view-mode)))
      ("Prog" (and (derived-mode . prog-mode)
                   (not (starred-name))))
      ("Dired" (mode . dired-mode))
      ("IRC" (or (mode . rcirc-mode)
                 (mode . erc-mode)))
      ("Images" (or (mode . image-mode)
                    (mode . image-dired-display-image-mode)
                    (mode . image-dired-thumbnail-mode))))))
  :config
  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t (format "%8d" (buffer-size)))))

  ;; Modify the default ibuffer-formats
  (setq ibuffer-formats
        '((mark modified read-only " "
                (name 18 18 :left :elide)
                " "
                (size-h 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                filename-and-process))))

(es/ctrl-c-keys
  ;; ivy
  "i"   '(:ignore t :which-key "ivy")
  "ic"  '(counsel-compile :which-key "compile")
  "ig"  '(counsel-git :which-key "git")
  "ij"  '(counsel-git-grep :which-key "git grep")
  "iL"  '(counsel-git-log :which-key "git log")
  "ik"  '(counsel-rg :which-key "rg")
  "im"  '(counsel-linux-app :which-key "linux app")
  "in"  '(counsel-fzf :which-key "fzf")
  "iJ"  '(counsel-file-jump :which-key "file jump")
  "iw"  '(counsel-wmctrl :which-key "wmctrl")
  "ir"  '(ivy-resume :which-key "ivy resume")
  "ib"  '(counsel-bookmark :which-key "bookmark")
  "id"  '(counsel-descbinds :which-key "descbinds")
  "io"  '(counsel-outline :which-key "outline")
  "iF"  '(counsel-org-file :which-key "org file"))

(provide 'init-buffer)
;;; init-buffer.el ends here
