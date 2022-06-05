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
  (company-tooltip-idle-delay 10)
  (company-require-match nil)
  (company-show-quick-access t)

  (company-minimum-prefix-length 3)
  (company-tooltip-width-grow-only t)
  (company-tooltip-align-annotations t)
  (company-dabbrev-code-everywhere t)
  (company-tempo-expand t)

  (company-frontends
   '(
     company-pseudo-tooltip-unless-just-one-frontend-with-delay
     company-preview-frontend
     company-echo-metadata-frontend
     ))

  (company-backends
   '(company-capf
     company-dabbrev-code
     company-dabbrev
     company-files)))

(use-package company-box
  :hook (company-mode . company-box-mode)
  :custom
  (company-box-frame-behavior 'point)
  (company-box-icons-alist 'company-box-icons-images))

;; lsp-mode
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  ((
    c-mode
    c++-mode
    typescript-mode
    js-mode
    python-mode
    arduino-mode) . lsp-deferred)
  :bind (
         :map lsp-mode-map
         ("TAB" . completion-at-point))
  :config
  (setq lsp-enable-symbol-highlighting t
        lsp-lens-enable t

        lsp-modeline-code-actions-enable t
        lsp-modeline-diagnostics-mode t

        lsp-dired-mode t

        lsp-headerline-breadcrumb-enable-symbol-numbers t
        lsp-file-watch-threshold 2000

        lsp-clangd-binary-path "d:/LLVM/bin/clangd.exe"))

(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  ;; lsp ui sideline
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-show-code-actions t)

  ;; lsp ui peek
  (lsp-ui-peek-always-show t)

  ;; lsp ui doc
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-show-with-cursor t)
  ;;(lsp-ui-doc-position 'at-point)

  ;; lsp ui imenu
  (lsp-ui-imenu-auto-refresh 'after-save)
  (lsp-ui-imenu--custom-mode-line-format t))

(use-package lsp-treemacs
  :after lsp-mode
  :config
  (lsp-treemacs-sync-mode 1))

(use-package dap-mode
  :commands dap-debug
  :hook ((python-mode . dap-mode)
         (c-mode . dap-mode))
  :config
  (dap-mode 1)
  (dap-auto-configure-mode)

  (dap-ui-mode 1)
  (dap-ui-controls-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)

  (require 'dap-hydra)

  (require 'dap-python)
  (setq dap-python-debugger 'debugpy)

  (require 'dap-lldb)
  (require 'dap-gdb-lldb)
  ;; 安装 .extendsion/vscode
  ;;(dap-gdb-lldb-setup)
  (setq dap-lldb-debug-program '("d:/LLVM/bin/lldb-vscode.exe")
        dap-gdb-lldb-debug-program '("d:/LLVM/bin/lldb-vscode.exe")
        dap-gdb-lldb-path-lldb '("d:/LLVM/bin/lldb-vscode.exe")
        )
  )

(use-package posframe)

(use-package lsp-bridge
  :straight (lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge" :file ("*"))
  :after lsp-mode
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

;;; program
(use-package arduino-mode)

(defhydra hydra-program (
                         :color pink
                         :exit t
                         :hint nil)
  "
  ^format^             ^lsp UI^                ^lsp treemacs^
--------------------------------------------------------------------
_a_: code format     _ld_: definition        _le_: errors list
  ^ ^                _lr_: references        _lq_: quick fix
  ^ ^                _lt_: type def          _ls_: symbols
  ^ ^                _li_: imenu             _lf_: references
  ^ ^                   ^ ^                  _lp_: implementations

"
  ("a" format-all-buffer)

  ;; lsp ui
  ("ld" lsp-ui-peek-find-definitions)
  ("lr" lsp-ui-peek-find-references)
  ("lt" lsp-find-type-definition)
  ("li" lsp-ui-imenu)

  ;; lsp treemacs
  ("le" lsp-treemacs-errors-list)
  ("lq" lsp-treemacs-quick-fix)
  ("ls" lsp-treemacs-symbols)
  ("lf" lsp-treemacs-references)
  ("lp" lsp-treemacs-implementations)

  ("q" nil "quit" :color red))

(hkk/leader-key
  ;; hydra keybindings
  "p" '(hydra-program/body :which-key "program")
  "d" '(dap-hydra/body :which-key "dap debug"))

(hkk/ctrl-c
  ;; flycheck
  "!"  '(:ignore t :which-key "flycheck"))

(provide 'init-program)
;;; init-program.el ends here
