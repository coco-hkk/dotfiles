;;; init-org.el --- settings for org -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Turn on indentation and auto-fill mode for Org files
(defun hkk/org-mode-setup ()
  "Org Mode Setup."
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1))

;; font settings
(defun hkk/org-font-setup ()
  "Org Font Setup."
  (set-face-attribute 'org-document-title nil :font "ubuntu mono" :weight 'bold :height 1.4)

  (dolist (face '((org-level-1 . 1.3)
                  (org-level-2 . 1.25)
                  (org-level-3 . 1.2)
                  (org-level-4 . 1.1)))
    (set-face-attribute (car face) nil :font "ubuntu mono" :weight 'medium :height (cdr face)))

  (require 'org-indent)

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block   nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table   nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code    nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-indent  nil :inherit '(org-hide fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox  nil :inherit 'fixed-pitch)

  ;; Get rid of the background on column views
  (set-face-attribute 'org-column       nil :background nil)
  (set-face-attribute 'org-column-title nil :background nil))

(use-package org
  :straight (:type built-in)
  :after evil-collection
  :config
  (hkk/org-mode-setup)
  (hkk/org-font-setup)

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

  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path t)

  ;; org babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t))))

;; org template
(use-package org-tempo
  :after org
  :straight (:type built-in)
  :config
  (add-to-list 'org-structure-template-alist '("sh" . "src sh"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("li" . "src lisp"))
  (add-to-list 'org-structure-template-alist '("sc" . "src scheme"))
  (add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("go" . "src go"))
  (add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
  (add-to-list 'org-structure-template-alist '("json" . "src json")))

(use-package toc-org
  :hook (org-mode . toc-org-mode))

(use-package evil-org
  :hook ((org-mode
          org-agenda-mode) . evil-org-mode)
  :config
  (evil-org-set-key-theme '(navigation
                            todo
                            insert
                            textobjects
                            additional)))

(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-remove-leading-stars t))

;; edit org like document
(use-package visual-fill-column
  :hook (org-mode . (lambda ()
                      (visual-fill-column-mode 1)
                      (setq visual-fill-column-width 100
                            visual-fill-column-center-text t))))

;; org present settings
(defun hkk/org-present-prepare-slide ()
  "Org present slide settings."
  (org-overview)
  (org-show-entry)
  (org-show-children))

(defun hkk/org-present-hook ()
  "Org present hook settings."
  (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
                                     (header-line (:height 4.5) variable-pitch)
                                     (org-document-title (:height 1.75) org-document-title)
                                     (org-code (:height 1.55) org-code)
                                     (org-verbatim (:height 1.55) org-verbatim)
                                     (org-block (:height 1.25) org-block)
                                     (org-block-begin-line (:height 0.7) org-block)))
  (setq header-line-format " ")
  (org-present-hide-cursor)
  (org-display-inline-images)
  (org-present-read-only))

(defun hkk/org-present-quit-hook ()
  "Org present quit hook settings."
  (setq-local face-remapping-alist '((default variable-pitch default)))
  (setq header-line-format nil)
  (org-present-small)
  (org-remove-inline-images)
  (org-present-show-cursor)
  (org-present-read-write))

(defun hkk/org-present-prev ()
  "Org present prev settings."
  (interactive)
  (org-present-prev)
  (hkk/org-present-prepare-slide))

(defun hkk/org-present-next ()
  "Org present next settings."
  (interactive)
  (org-present-next)
  (hkk/org-present-prepare-slide))

(use-package org-present
  :bind (:map org-present-mode-keymap
              ("C-c j" . hkk/org-present-next)
              ("C-c k" . hkk/org-present-prev))
  :hook
  (org-present-mode . hkk/org-present-hook)
  (org-present-mode-quit . hkk/org-present-quit-hook))

(defhydra hydra-org (
                     :color pink
                     :exit t
                     :hint nil)
  "
                                ^Org mode^
---------------------------------------------------------------------------
_a_: agenda status    _c_: capture    _t_: todo list    _n_: toggle subtree
_i_: insert link      _x_: export     _p_: present      _q_: present quit

                                ^Org Roam^
---------------------------------------------------------------------------
   ^Dailies^          ^Capture^          ^Jump^
_rt_: today        _rT_: today        _rm_: current month
_rr_: tomorrow     _rR_: tomorrow     _re_: current year
_ry_: yesterday    _rY_: yesterday      ^ ^
_rd_: date            ^ ^               ^ ^

   ^Others^
_ri_: node insert       _rI_: insert immediate    _ra_: completion
_rl_: buffer toggle     _rf_: node find
_rc_: capture           _rg_: graph
_ru_: UI open           _rU_: UI mode

"
  ;; org mode
  ("a" org-agenda)
  ("c" org-capture)
  ("t" org-todo-list)
  ("x" org-export-dispatch)
  ("p" org-present)
  ("q" org-present-quit)
  ("i" org-insert-link)
  ("n" org-toggle-narrow-to-subtree)

  ;; org roam
  ("rt" org-roam-dailies-goto-today)
  ("rr" org-roam-dailies-goto-tomorrow)
  ("ry" org-roam-dailies-goto-yesterday)
  ("rd" org-roam-dailies-goto-date)
  ("rT" org-roam-dailies-capture-today)
  ("rR" org-roam-dailies-capture-tomorrow)
  ("rY" org-roam-dailies-capture-yesterday)
  ("rm" dw/org-roam-goto-month)
  ("re" dw/org-roam-goto-year)

  ("ri" org-roam-node-insert)
  ("rI" org-roam-insert-immediate)
  ("rl" org-roam-buffer-toggle)
  ("rf" org-roam-node-find)
  ("rc" org-roam-capture)
  ("rg" org-roam-graph)

  ("ru" org-roam-ui-open)
  ("rU" org-roam-ui-mode)

  ("ra" completion-at-point)

  ("q" nil "quit" :color pink))

(hkk/leader-key
  ;; hydra keybindings
  "o" '(hydra-org/body :which-key "org"))

(provide 'init-org)
;;; init-org.el ends here
