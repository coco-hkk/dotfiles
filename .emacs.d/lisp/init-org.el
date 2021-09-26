;;; init-org.el --- settings for org -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun dw/org-path (path)
  (expand-file-name path "f:/Notes/org/"))

;; Turn on indentation and auto-fill mode for Org files
(defun es/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil)
  (diminish org-indent-mode))

;; font settings
(defun es/org-font-setup ()
  "Org font setup"
  (set-face-attribute 'org-document-title nil :font "Cantarell" :weight 'bold :height 1.3)

  (dolist (face '((org-level-1 . 1.728)
                  (org-level-2 . 1.44)
                  (org-level-3 . 1.2)
                  (org-level-4 . 1.1)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'medium :height (cdr face)))

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
  :hook (org-mode . es/org-mode-setup)
  :config
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

  (push '("conf-unix" . conf-unix) org-src-lang-modes)

  (es/org-font-setup)

  (use-package org-superstar
    :hook (org-mode . org-superstar-mode)
    :custom
    (org-superstar-remove-leading-stars t))

  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src sh"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("li" . "src lisp"))
  (add-to-list 'org-structure-template-alist '("sc" . "src scheme"))
  (add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("go" . "src go"))
  (add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
  (add-to-list 'org-structure-template-alist '("json" . "src json"))

  (use-package evil-org
    :hook ((org-mode . evil-org-mode)
           (org-agenda-mode . evil-org-mode)
           (evil-org-mode . (lambda () (evil-org-set-key-theme '(navigation todo insert textobjects additional)))))
    :config
    (require 'evil-org-agenda)
    (evil-org-agenda-set-keys))

  ;; Workflow States
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
          (sequence "|" "WAIT(w)" "BACK(b)")))

  (setq org-todo-keyword-faces
        '(("NEXT" . (:foreground "orange red" :weight bold))
          ("WAIT" . (:foreground "HotPink2" :weight bold))
          ("BACK" . (:foreground "MediumPurple3" :weight bold))))

  (setq org-tag-alist
        '((:startgroup)
                                        ; Put mutually exclusive tags here
          (:endgroup)
          ("@home" . ?H)
          ("@work" . ?W)
          ("batch" . ?b)
          ("followup" . ?f)))

  ;; Configure custom agenda views
  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-span 'day)
  (setq org-agenda-start-with-log-mode t)

  ;; Make done tasks show up in the agenda log
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-columns-default-format "%20CATEGORY(Category) %65ITEM(Task) %TODO %6Effort(Estim){:}  %6CLOCKSUM(Clock) %TAGS")

  (setq org-agenda-custom-commands
        `(("d" "Dashboard"
           ((agenda "" ((org-deadline-warning-days 7)))
            (tags-todo "+PRIORITY=\"A\""
                       ((org-agenda-overriding-header "High Priority")))
            (tags-todo "+followup" ((org-agenda-overriding-header "Needs Follow Up")))
            (todo "NEXT"
                  ((org-agenda-overriding-header "Next Actions")
                   (org-agenda-max-todos nil)))
            (todo "TODO"
                  ((org-agenda-overriding-header "Unprocessed Inbox Tasks")
                   (org-agenda-files '(,(dw/org-path "Inbox.org")))
                   (org-agenda-text-search-extra-files nil)))))

          ("n" "Next Tasks"
           ((agenda "" ((org-deadline-warning-days 7)))
            (todo "NEXT"
                  ((org-agenda-overriding-header "Next Tasks")))))

          ;; Low-effort next actions
          ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
           ((org-agenda-overriding-header "Low Effort Tasks")
            (org-agenda-max-todos 20)
            (org-agenda-files org-agenda-files)))))

  (add-hook 'org-timer-set-hook #'org-clock-in)

(defun dw/get-todays-journal-file-name ()
  "Gets the journal file name for today's date"
  (interactive)
  (let* ((journal-file-name
           (expand-file-name
             (format-time-string "%Y/%Y-%2m-%B.org")
             (dw/org-path "Journal/")))
         (journal-year-dir (file-name-directory journal-file-name)))
    (if (not (file-directory-p journal-year-dir))
      (make-directory journal-year-dir))
    journal-file-name))

(defun dw/on-org-capture ()
  ;; Don't show the confirmation header text
  (setq header-line-format nil)

  ;; Control how some buffers are handled
  (let ((template (org-capture-get :key t)))
    (pcase template
      ("jj" (delete-other-windows)))))

(add-hook 'org-capture-mode-hook 'dw/on-org-capture)

(setq org-capture-templates
  `(("t" "Tasks")
    ("tt" "Task" entry (file ,(dw/org-path "Inbox.org"))
         "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)
    ("ts" "Clocked Entry Subtask" entry (clock)
         "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

    ("j" "Journal Entries")
    ("je" "General Entry" entry
         (file+olp+datetree ,(dw/org-path "Journal.org"))
         "\n* %<%I:%M %p> - %^{Title} \n\n%?\n\n"
         :tree-type week
         :clock-in :clock-resume
         :empty-lines 1)
    ("jt" "Task Entry" entry
         (file+olp+datetree ,(dw/org-path "Journal.org"))
         "\n* %<%I:%M %p> - Task Notes: %a\n\n%?\n\n"
         :tree-type week
         :clock-in :clock-resume
         :empty-lines 1)
    ("jj" "Journal" entry
         (file+olp+datetree ,(dw/org-path "Journal.org"))
         "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
         :tree-type week
         :clock-in :clock-resume
         :empty-lines 1)))

  (define-key global-map (kbd "C-c j")
    (lambda () (interactive) (org-capture nil "jj"))))

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

;;; org roam settings
(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "F:/Notes/org-notes")
  (org-dilies-directory "f:/Notes/org-notes/daily")
  (org-roam-graph-executable "D:/Graphviz/bin/dot.exe")
  (org-roam-completion-everywhere t)
  :config
  (org-roam-db-autosync-mode)

  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(use-package org-roam-ui
  :straight
  (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))

  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))


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

(es/ctrl-c-keys
  ;; org roam
  "r"   '(:ignore t :which-key "org roam")
  "rl"  '(org-roam-buffer-toggle :which-key "toggle")
  "ri"  '(org-roam-node-insert :which-key "node insert")
  "rf"  '(org-roam-node-find :which-key "node find")
  "rc"  '(org-roam-capture :which-key "capture")
  "rj"  '(org-roam-dailies-capture-today :which-key "capture today")
  "rt"  '(org-roam-dailies-goto-today :which-key "daily today")
  "ry"  '(org-roam-dailies-goto-yesterday :which-key "daily yesterday")
  "rr"  '(org-roam-dailies-goto-tomorrow :which-key "daily tomorrow")
  "rg"  '(org-roam-graph :which-key "graph")

  "ru"  '(org-roam-ui-mode :which-key "ui"))

(provide 'init-org)
;;; init-org.el ends here
