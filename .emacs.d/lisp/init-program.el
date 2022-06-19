;;; init-program.el --- settings for lsp -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package posframe)

(use-package lsp-bridge
  :straight nil
  :load-path "site-lisp/lsp-bridge"
  :hook (prog-mode . lsp-bridge-mode)
  :config
  (setq lsp-bridge-completion-candidates t
        lsp-bridge-enable-signature-help t
        lsp-bridge-enable-log t))

(use-package acm
  :straight nil
  :load-path "site-lisp/lsp-bridge/acm"
  :bind (:map acm-mode-map
              ("C-j" . acm-select-next)
              ("C-k" . acm-select-prev))
  :config
  (acm-doc-show)
  (setq acm-mode t
        acm-enable-dabbrev t
        acm-backend-elisp-min-length 4
        acm-backend-tempel-candidates-number 4))

;; 语法检查
;; 需要安装语法检查工具，如 pylint 和 eslint
(use-package flycheck
  :hook (prog-mode . flycheck-mode))

(use-package with-venv)

(use-package dap-mode
  :commands dap-debug
  :hook ((python-mode . dap-ui-mode)
         (python-mode . dap-mode)
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
  (defun dap-python--pyenv-executable-find (command)
    (with-venv (executable-find "python")))

  (require 'dap-lldb)
  (require 'dap-gdb-lldb)
  ;; 安装 .extendsion/vscode
  ;;(dap-gdb-lldb-setup)
  (setq dap-lldb-debug-program '("d:/LLVM/bin/lldb-vscode.exe")
        dap-gdb-lldb-debug-program '("d:/LLVM/bin/lldb-vscode.exe")
        dap-gdb-lldb-path-lldb '("d:/LLVM/bin/lldb-vscode.exe")
        )
  )

(use-package format-all
  :hook
  (prog-mode . format-all-mode))

(provide 'init-program)
;;; init-program.el ends here
