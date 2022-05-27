;;; init-org-roam.el --- settings for org -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq org-roam-directory "f:/GitHub/roam/note")
(setq org-roam-dailies-directory "f:/GitHub/roam/daily")
(setq org-roam-graph-executable "d:/Graphviz/bin/dot.exe")

(use-package org-roam
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-completion-everywhere t)
  (org-roam-completion-system 'default)

  (org-roam-capture-templates
   '(("d" "default" plain
      #'org-roam-capture--get-point
      "%?"
      :file-name "${slug}.org"
      :head "#+title: ${title}\n"
      :unnarrowed t)
     ("ll" "link note" plain
      #'org-roam-capture--get-point
      "* %^{Link}"
      :file-name "Inbox"
      :olp ("Links")
      :unnarrowed t
      :immediate-finish)
     ("lt" "link task" entry
      #'org-roam-capture--get-point
      "* TODO %^{Link}"
      :file-name "Inbox"
      :olp ("Tasks")
      :unnarrowed t
      :immediate-finish)))

  (org-roam-dailies-capture-templates
   '(("d" "default" entry
      #'org-roam-capture--get-point
      "* %?"
      :file-name "daily/%<%Y-%m-%d>"
      :head "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")
     ("t" "Task" entry
      #'org-roam-capture--get-point
      "* TODO %?\n  %U\n  %a\n  %i"
      :file-name "daily/%<%Y-%m-%d>"
      :olp ("Tasks")
      :empty-lines 1
      :head "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")
     ("j" "journal" entry
      #'org-roam-capture--get-point
      "* %<%I:%M %p> - Journal  :journal:\n\n%?\n\n"
      :file-name "daily/%<%Y-%m-%d>"
      :olp ("Log")
      :head "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")
     ("l" "log entry" entry
      #'org-roam-capture--get-point
      "* %<%I:%M %p> - %?"
      :file-name "daily/%<%Y-%m-%d>"
      :olp ("Log")
      :head "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")
     ("m" "meeting" entry
      #'org-roam-capture--get-point
      "* %<%I:%M %p> - %^{Meeting Title}  :meetings:\n\n%?\n\n"
      :file-name "daily/%<%Y-%m-%d>"
      :olp ("Log")
      :head "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")))

  :bind (("C-c o l" . org-roam-buffer-toggle)
         ("C-c o f" . org-roam-node-find)
         ("C-c o i" . org-roam-node-insert)
         ("C-c o c" . org-roam-capture)
         ("C-c o g" . org-roam-graph)
         ("C-M-i" . completion-at-point)
         :map org-roam-mode-map
         (("C-c o l"   . org-roam)
          ("C-c o f"   . org-roam-find-file)
          ("C-c o d"   . org-roam-dailies-find-date)
          ("C-c o c"   . org-roam-dailies-capture-today)
          ("C-c o C r" . org-roam-dailies-capture-tomorrow)
          ("C-c o t"   . org-roam-dailies-find-today)
          ("C-c o y"   . org-roam-dailies-find-yesterday)
          ("C-c o r"   . org-roam-dailies-find-tomorrow))
         :map org-mode-map
         (("C-c o i" . org-roam-insert)
          ("C-c o I" . org-roam-insert-immediate)))
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
        org-roam-ui-open-on-start nil))

;; 查找 org roam
(use-package deft
  :commands (deft)
  :bind
  ("C-c o d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-extensions '("md" "org"))
  (deft-directory org-roam-directory))

(use-package org-roam-bibtex
  :after (org-roam)
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  ; (require 'org-ref))
  (setq orb-preformat-keywords
        '("=key=" "title" "url" "file" "author-or-editor" "keywords"))
  (setq orb-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           ""
           :file-name "literature/%<%Y-%m-%d-%H-%M-%S>-${slug}"
           :head "#+TITLE: ${=key=}: ${title}
#+ROAM_KEY: ${ref}
#+ROAM_TAGS:
Time-stamp: <>
- tags :: ${keywords}

* ${title}
  :PROPERTIES:
  :Custom_ID: ${=key=}
  :URL: ${url}
  :AUTHOR: ${author-or-editor}
  :NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")
  :NOTER_PAGE:
  :END:

"

           :unnarrowed t))))



;;自动创建笔记的创建时间和修改时间
(use-package org-roam-timestamps
  :straight (:type git :host github :repo "ThomasFKJorna/org-roam-timestamps")
  :after org-roam
  :config
  (org-roam-timestamps-mode))

;;跨文件的引用，能够实现笔记的一处修改，处处修改。
(use-package org-transclusion
  :after org-roam
  :straight (org-transclusion :type git :host github :repo "nobiot/org-transclusion"))

(hkk/ctrl-c
  ;; org roam
  "o"   '(:ignore t :which-key "org roam"))

(provide 'init-org-roam)
;;; init-org-roam.el ends here
