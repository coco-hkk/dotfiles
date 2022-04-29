;;; init-org.el --- settings for org -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Turn on indentation and auto-fill mode for Org files
(defun es/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil)

  ; (diminish org-indent-mode)
  )

;; font settings
(defun es/org-font-setup ()
  "Org Font Setup"
  (set-face-attribute 'org-document-title nil :font "ubuntu mono" :weight 'bold :height 1.3)

  (dolist (face '((org-level-1 . 1.25)
                  (org-level-2 . 1.2)
                  (org-level-3 . 1.15)
                  (org-level-4 . 1.1)
                  (org-level-5 . 1.05)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "ubuntu mono" :weight 'medium :height (cdr face)))

  (require 'org-indent)

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

  ;; Get rid of the background on column views
  (set-face-attribute 'org-column nil :background nil)
  (set-face-attribute 'org-column-title nil :background nil))

(use-package org
  :hook org-mode
  :config
  (es/org-mode-setup)
  (es/org-font-setup)

  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t
        org-src-fontify-natively t
        org-fontify-quote-and-verse-blocks t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 2
        org-hide-block-startup nil
        org-src-preserve-indentation nil
        org-startup-folded 'content
        org-cycle-separator-lines 2)

  ;; org refile
  (setq org-refile-targets
        '((nil :maxlevel . 1)
          ("org-agenda-files" :maxlevel . 1)))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  ;; org babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)))

  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src sh"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("li" . "src lisp"))
  (add-to-list 'org-structure-template-alist '("sc" . "src scheme"))
  (add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("go" . "src go"))
  (add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
  (add-to-list 'org-structure-template-alist '("json" . "src json")))

(use-package evil-org
  :after org
  :hook ((org-mode . evil-org-mode)
         (org-agenda-mode . evil-org-mode)
         (evil-org-mode . (lambda () (evil-org-set-key-theme '(navigation todo insert textobjects additional))))))

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-remove-leading-stars t))

;; edit org like document
(use-package visual-fill-column
  :hook (org-mode . (lambda ()
                      (setq visual-fill-column-width 100)
                      (setq visual-fill-column-center-text t)
                      (visual-fill-column-mode 1))))

;; org present settings
(defun es/org-present-prepare-slide ()
  (org-overview)
  (org-show-entry)
  (org-show-children))

(defun es/org-present-hook ()
  (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
                                     (header-line (:height 4.5) variable-pitch)
                                     (org-document-title (:height 1.75) org-document-title)
                                     (org-code (:height 1.55) org-code)
                                     (org-verbatim (:height 1.55) org-verbatim)
                                     (org-block (:height 1.25) org-block)
                                     (org-block-begin-line (:height 0.7) org-block)))
  (org-present-hide-cursor)
  (org-display-inline-images)
  (org-present-read-only))

(defun es/org-present-quit-hook ()
  (setq-local face-remapping-alist '((default variable-pitch default)))
  (org-remove-inline-images)
  (org-present-show-cursor)
  (org-present-read-write))

(defun es/org-present-prev ()
  (interactive)
  (org-present-prev)
  (es/org-present-prepare-slide))

(defun es/org-present-next ()
  (interactive)
  (org-present-next)
  (es/org-present-prepare-slide))

(use-package org-present
  :after org
  :bind (:map org-present-mode-keymap
              ("C-c C-j" . es/org-present-next)
              ("C-c C-k" . es/org-present-prev))
  :hook ((org-present-mode . es/org-present-hook)
         (org-present-mode-quit . es/org-present-quit-hook)))

(use-package toc-org
  :hook (org-mode . toc-org-mode))

;; org key binding
(es/leader-key-def
  ;; org mode
  "o"   '(:ignore t :which-key "org mode")

  "oi"  '(:ignore t :which-key "insert")
  "oil" '(org-insert-link :which-key "insert link")

  "on"  '(org-toggle-narrow-to-subtree :which-key "toggle narrow")

  "os"  '(dw/counsel-rg-org-files :which-key "search notes")

  "oa"  '(org-agenda :which-key "agenda status")
  "oc"  '(org-capture t :which-key "capture")
  "ot"  '(org-todo-list :which-key "todos")
  "ox"  '(org-export-dispatch t :which-key "export")

  "op"  '(org-present :which-key "present")
  "oq"  '(org-present-quit :which-key "present quit"))

(provide 'init-org)
;;; init-org.el ends here
