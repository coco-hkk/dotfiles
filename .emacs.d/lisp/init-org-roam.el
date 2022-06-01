;;; init-org-roam.el --- settings for org -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq org-roam-directory "f:/GitHub/roam/note")
(setq org-roam-dailies-directory "f:/GitHub/roam/dailies")
(setq org-roam-graph-executable "d:/Graphviz/bin/dot.exe")

(use-package org-roam
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

(defhydra hydra-org-roam (:color pink
                                 :exit t
                                 :hint nil)
  "
^Dailies^        ^Capture^       ^Jump^
^^^^^^^^-------------------------------------------------
_t_: today       _T_: today       _m_: current month
_r_: tomorrow    _R_: tomorrow    _e_: current year
_y_: yesterday   _Y_: yesterday   ^ ^
_d_: date        ^ ^              ^ ^

^Others^
^^^^^^^^-------------------------------------------------
_i_: node insert       _I_: insert immediate
_l_: buffer toggle     _f_: node find
_c_: capture           _g_: graph
_u_: UI open           _U_: UI mode

_a_: completion
"
  ("t" org-roam-dailies-goto-today)
  ("r" org-roam-dailies-goto-tomorrow)
  ("y" org-roam-dailies-goto-yesterday)
  ("d" org-roam-dailies-goto-date)
  ("T" org-roam-dailies-capture-today)
  ("R" org-roam-dailies-capture-tomorrow)
  ("Y" org-roam-dailies-capture-yesterday)
  ("m" dw/org-roam-goto-month)
  ("e" dw/org-roam-goto-year)

  ("i" org-roam-node-insert)
  ("I" org-roam-insert-immediate)
  ("l" org-roam-buffer-toggle)
  ("f" org-roam-node-find)
  ("c" org-roam-capture)
  ("g" org-roam-graph)

  ("u" org-roam-ui-open)
  ("U" org-roam-ui-mode)

  ("a" completion-at-point))

(hkk/leader-key
  ;; org roam
  "r" '(hydra-org-roam/body :which-key "org roam"))

(provide 'init-org-roam)
;;; init-org-roam.el ends here