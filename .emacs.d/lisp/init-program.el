;;; init-program.el --- settings for lsp -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; 语法检查
;; 需要安装语法检查工具，如 pylint 和 eslint
(use-package flycheck
  :hook (prog-mode . flycheck-mode))

;; 自动补全
(use-package company
  :hook (prog-mode . company-mode)
  :bind (
         :map company-mode-map
         ([remap completion-at-point] . company-complete)
         :map company-active-map
         ([tab]     . company-complete-common-or-cycle)
         ([backtab] . company-select-previous-or-abort))
  :custom
  (company-idle-delay 0)
  (company-require-match nil)
  (company-show-quick-access t)

  (company-minimum-prefix-length 3)
  (company-tooltip-width-grow-only t)
  (company-tooltip-align-annotations t)
  (company-dabbrev-code-everywhere t)
  (company-tempo-expand t)

  (company-frontends
   '(company-preview-frontend
     company-echo-metadata-frontend))

  (company-backends
   '(company-capf
     company-files
     company-dabbrev)))

(use-package company-box
  :hook (company-mode . company-box-mode)
  :custom
  (company-box-icons-alist 'company-box-icons-idea))

;; lsp-mode
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((typescript-mode js2-mode web-mode) . lsp)
  :bind (:map lsp-mode-map
              ("TAB" . completion-at-point))
  :custom
  (lsp-headerline-breadcrumb-enable nil) ;; keep headline clean
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :after lsp
  :hook (lsp-mode . lsp-ui-mode)
  :custom-face
  (lsp-ui-sideline-symbol ((t (:foreground "grey" :box (:line-width (1 . -1) :color "grey") :height 0.99))))
  :custom
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-actions-icon "")

  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-use-webkit t)
  (lsp-ui-doc-show))

(use-package posframe)

(use-package lsp-bridge
  :straight (lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge" :file ("*"))
  :custom
  (lsp-bridge-completion-provider 'company)
  :config
  (require 'lsp-bridge-icon)
  (global-lsp-bridge-mode)

  ;; For Xref support
  (add-hook 'lsp-bridge-mode-hook (lambda ()
                                    (add-hook 'xref-backend-functions #'lsp-bridge-xref-backend nil t))))

(use-package format-all
  :hook
  (prog-mode . format-all-mode)
  :custom
  (format-all-show-errors 'warnings))

(defhydra hydra-program (:color pink
                                :exit t
                                :hint nil)
  "
^format^           |                 ^lsp^
^^^^^^^-------------------------------------------------------
_a_: code format   |   _d_: definition       _s_: doc show
  ^ ^              |   _r_: references       _h_: doc hide
  ^ ^              |   _t_: type def         _i_: imenu
"
  ("a" format-all-buffer)

  ("d" lsp-find-definition)
  ("r" lsp-find-references)
  ("t" lsp-find-type-definition)
  ("s" lsp-ui-doc-show)
  ("h" lsp-ui-doc-hide)
  ("i" lsp-ui-imenu))

(hkk/leader-key
  ;; hydra keybindings
  "c" '(hydra-program/body :which-key "program"))

(hkk/ctrl-c
  ;; flycheck
  "!"  '(:ignore t :which-key "flycheck"))

(provide 'init-program)
;;; init-program.el ends here