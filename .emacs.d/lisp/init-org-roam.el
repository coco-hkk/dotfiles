;;; init-org-roam.el --- settings for org -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq org-roam-directory "f:/GitHub/roam/note"
      org-roam-dailies-directory "f:/GitHub/roam/dailies"
      org-roam-graph-executable "d:/Graphviz/bin/dot.exe")

(use-package org-roam
  :defer t
  :custom
  (org-roam-completion-everywhere t)

  (org-roam-capture-templates
   '(("n" "note" plain
      "%?"
      :if-new (file+head "${slug}.org"
                         "#+title: ${title}\n#+date: %u\n")
      :unnarrowed t)
     ("a" "article" plain "%?"
      :if-new
      (file+head "${title}.org" "#+title: ${title}\n#+filetags: :article:\n")
      :unnarrowed t)))

  (org-roam-dailies-capture-templates
   '(("d" "default" entry
      "* %?"
      :target (file+head "%<%Y-%m-%d>.org"
                         "#+title: %<%Y-%m-%d>\n"))))
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

;; 查找 org roam
(use-package deft
  :after org-roam
  :commands (deft)
  :bind
  ("C-c o d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-extensions '("md" "org"))
  (deft-directory org-roam-directory))

(use-package org-roam-bibtex
  :after org-roam
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

(provide 'init-org-roam)
;;; init-org-roam.el ends here
