;;; init-bindings.el --- settings for keybindings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; msic
;;; Commentary:
;;; Code:

(defun with-faicon (icon str &optional height v-adjust)
  (s-concat (all-the-icons-faicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

;;; global variable
(setq misc--title           (with-faicon "cog" "Misc")
      toggles--title        (with-faicon "toggle-on" "Toggles")
      window--title         (with-faicon "windows" "Window Management")
      completion--title     (with-faicon "expand" "Completion")
      eaf--title            (with-faicon "film" "EAF")
      lsp--title            (with-faicon "code" "Lsp Bridge")
      treemacs--title       (with-faicon "tree" "Treemacs")
      org--title            (with-faicon "book" "Org")
      )

;;; misc
(pretty-hydra-define misc-hydra
  (:title misc--title :quit-key "q")
  ("Jampal"
   (("je" read-aloud-change-engine "Change Engines")
    ("jr" read-aloud-buf "Read Buffer")
    ("jt" read-aloud-this "Read This")
    ("js" read-aloud-stop "Stop"))

   "Benchmark"
   (("bb" benchmark-init/show-durations-tabulated "Show Table")
    ("bt" benchmark-init/show-durations-tree "Show Tree"))

   "Fanyi"
   (("yd" fanyi-dwim "Dwin")
    ("ye" fanyi-dwim2 "Dwin2")
    ("yh" fanyi-from-history "History")
    ("yq" fanyi-copy-query-word "Copy & Query"))

   "Other"
   (("ff" (lambda () (interactive) (find-file (expand-file-name "~/.emacs.d/init.el"))) "init.el")
    ("fl" (lambda () (interactive) (dired (expand-file-name "~/.emacs.d/lisp/"))) "lisp")
    ("s" eshell-toggle "Toggle Eshell")
    ("i" eshell-command-insert "Eshell result")
    ("p" proced "Proced"))

   "Emms"
   (("el" emms-playlist-mode-switch-buffer "播放列表")
    ("ed" emms-add-directory-tree "媒体库")
    ("ej" emms-previous "上一首")
    ("ek" emms-next "下一首")
    ("er" emms-toggle-repeat-playlist "列表重播")
    ("es" emms-toggle-random-playlist "列表随机"))
   ))

;;; toggle
(pretty-hydra-define toggles-hydra
  (:title toggles--title :quit-key "q")
  ("Basic"
   (("v" view-mode "view number" :toggle t)
    ("n" linum-mode "line number" :toggle t)
    ("w" whitespace-mode "whitespace" :toggle t)
    ("W" whitespace-cleanup "whitespace cleanup" :toggle t)
    ("r" rainbow-mode "rainbow" :toggle t))

   "Line & Column"
   (("L" toggle-truncate-lines "truncate line" :toggle t)
    ("N" display-line-numbers-mode "line number" :toggle t)
    ("f" display-fill-column-indicator-mode "column indicator" :toggle t)
    ("c" visual-fill-column-mode "visual column" :toggle t))

   "Highlight"
   (("l" hl-line-mode "line" :toggle t))

   "Coding"
   (("F" flycheck-mode "flycheck" :toggle t))

   "Emacs"
   (("D" toggle-debug-on-error "debug on error" :toggle (default-value 'debug-on-error))
    ("X" toggle-debug-on-quit "debug on quit" :toggle (default-value 'debug-on-quit)))))

;;; window
(pretty-hydra-define window-hydra
  (:title window--title :quit-key "q")
  ("Actions"
   (("TAB" other-window "switch")
    ("x" ace-delete-window "delete")
    ("m" ace-delete-other-windows "maximize")
    ("s" ace-swap-window "swap")
    ("a" ace-select-window "select"))

   "Resize"
   (("n" balance-windows "balance")
    ("f" toggle-frame-fullscreen "toggle fullscreen"))

   "Split"
   (("b" split-window-right "horizontally")
    ("v" split-window-below "vertically"))

   "Text Scale"
   (("g" text-scale-increase "in")
    ("l" text-scale-decrease "out"))))

;;; completion
(pretty-hydra-define completion-hydra
  (:title completion--title :quit-key "q")
  ("Consult"
   (("cp" consult-grep "grep")
    ("cG" consult-git-grep "git grep")
    ("cr" consult-ripgrep "ripgrep")
    ("cl" consult-line "line")
    ("cL" consult-line-multi "line multi")
    ("cm" consult-keep-lines "multi occur")
    ("ck" consult-focus-lines "keep lines")
    ("cu" consult-multi-occur "focus lines")
    ("cb" consult-bookmark "bookmark")
    ("cB" consult-project-buffer "project")
    ("ci" consult-imenu "imenu")
    ("cr" consult-recent-file "recent file")
    ("cc" consult-complex-command "complex cmd")
    ("cC" consult-mode-command "mode command")
    ("cK" consult-kmacro "macro"))
   ))

;;; eaf
(pretty-hydra-define eaf-hydra
  (:title eaf--title :quit-key "q")
  ("Browser"
   (("S" eaf-open-browser "browser")
    ("s" eaf-open-browser-with-history "search"))

   "Mindmap"
   (("m" eaf-create-mindmap "create")
    ("M" eaf-open-mindmap "open"))

   "File Manager"
   (("f" eaf-open-file-manager "manager"))

   "Open"
   (("o" eaf-open "open"))
   ))

;;; lsp
(pretty-hydra-define lsp-hydra
  (:title lsp--title :quit-key "q")
  ("Lsp Bridge"
   (("d" lsp-bridge-find-def-other-window "defintion other window")
    ("i" lsp-bridge-find-impl-other-window "implementation other window")
    ("r" lsp-bridge-find-references "reference")
    ("b" lsp-bridge-return-from-def "back calling")
    ("n" lsp-bridge-jump-to-next-diagnostic "diagnosis next")
    ("p" lsp-bridge-jump-to-prev-diagnostic "diagnosis pre"))

   "Acm"
   (("e" acm-toggle-english-helper "english helper"))
   ))

;;; treemacs
(pretty-hydra-define treemacs-hydra
  (:title treemacs--title :quit-key "q")
  ("Treemacs"
   (("t" treemacs "treemacs")
    ("s" treemacs-select-directory "select directory")
    ("S" treemacs-switch-workspace "switch workspace")
    ("n" treemacs-next-workspace "next workspace")
    ("e" treemacs-edit-workspace "edit workspace"))
   ))

;;; org
(pretty-hydra-define org-hydra
  (:title org--title :quit-key "q")
  ("Org Mode"
   (("oa" org-agenda "agenda status")
    ("oc" org-capture "cpature")
    ("ot" org-todo-list "todo list")
    ("on" org-toggle-narrow-to-subtree "toggle subtree")
    ("oi" org-insert-link "insert link")
    ("op" org-present "present")
    ("oq" org-present-quit "present quit")
    ("ox" org-export-dispatch "export")
    )

   "Roam Dailies"
   (("rt" org-roam-dailies-goto-today "today")
    ("rr" org-roam-dailies-goto-tomorrow "tomorrow")
    ("ry" org-roam-dailies-goto-yesterday "yesterday")
    ("rd" org-roam-dailies-goto-date "date"))

   "Roam Capture"
   (("rT" org-roam-dailies-capture-today "today")
    ("rR" org-roam-dailies-capture-tomorrow "tomorrow")
    ("rY" org-roam-dailies-capture-yesterday "yesterday"))

   "Roam UI"
   (("rg" org-roam-graph "graph")
    ("ru" org-roam-ui-open "open")
    ("rU" org-roam-ui-mode "UI mode"))

   "Roam Others"
   (("ri" org-roam-node-insert "node insert")
    ("rI" org-roam-insert-immediate "insert immediate")
    ("rf" org-roam-node-find "node find")
    ("rl" org-roam-buffer-toggle "buffer toggle")
    ("rc" org-roam-capture "capture"))
   ))

;;; general
(hkk/leader-key
  "M" '(misc-hydra/body :which-key "Misc")
  "T" '(toggles-hydra/body :which-key "Toggle")
  "c" '(completion-hydra/body :which-key "Completion")
  "d" '(dap-hydra/body :which-key "Dap")
  "e" '(eaf-hydra/body :which-key "EAF")
  "l" '(lsp-hydra/body :which-key "Lsp Bridge")
  "o" '(org-hydra/body :which-key "Org & Roam")
  "t" '(treemacs-hydra/body :which-key "Treemacs")
  "w" '(window-hydra/body :which-key "Window")
  )

(hkk/ctrl-c
  ;; flycheck
  "!"  '(:ignore t :which-key "flycheck")
  ;; yasnippet
  "&"  '(:ignore t :which-key "yasnippet")
  ;; projectile
  "p"   '(:ignore t :which-key "projectile"))

(provide 'init-bindings)
;;; init-bindings.el ends here
