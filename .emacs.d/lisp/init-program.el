;;; init-program.el --- settings for lsp -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package posframe)

(add-to-list 'load-path "d:/emacs/.emacs.d/site-lisp/lsp-bridge/")
(add-to-list 'load-path "d:/emacs/.emacs.d/site-lisp/lsp-bridge/acm/")

(require 'lsp-bridge)
(require 'acm)

(global-lsp-bridge-mode)
(setq lsp-bridge-completion-candidates t
      lsp-bridge-enable-signature-help t
      lsp-bridge-enable-log t)

(setq acm-mode t
      acm-enable-dabbrev t
      acm-backend-elisp-min-length 4
      acm-backend-tempel-candidates-number 4)
(acm-doc-show)

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

;;; program
(use-package arduino-mode)

(defhydra hydra-lsp-bridge (:color pink
                                :exit t
                                :hint nil)
  "
                           ^lsp bridge^
---------------------------------------------------------------------------
_fd_: definition        _dd_: lookup doc      _jn_: diagnostic next
_fi_: implementation    _dn_: pop doc down    _jp_: diagnostic pre
_fr_: reference         _dp_: pop doc up      _rn_: rename symbol
_fb_: back calling         ^ ^                _ip_: insert prefix candiates
_fo_: fd other win
_fw_: fi other win

                               ^acm^
---------------------------------------------------------------------------
_e_: english helper

"
  ("fd" lsp-bridge-find-def)
  ("fo" lsp-bridge-find-def-other-window)
  ("fi" lsp-bridge-find-impl)
  ("fw" lsp-bridge-find-impl-other-window)
  ("fr" lsp-bridge-find-references)
  ("fb" lsp-bridge-return-from-def)

  ("dd" lsp-bridge-lookup-documentation)
  ("dn" lsp-bridge-popup-documentation-scroll-down)
  ("dp" lsp-bridge-popup-documentation-scroll-up)

  ("rn" lsp-bridge-rename)

  ("jn" lsp-bridge-jump-to-next-diagnostic)
  ("jp" lsp-bridge-jump-to-prev-diagnostic)

  ("ip" lsp-bridge-insert-common-prefix)

  ("e" acm-toggle-english-helper)

  ("q" nil "quit" :color pink))

(hkk/leader-key
  ;; hydra keybindings
  "l" '(hydra-lsp-bridge/body :which-key "lsp bridge")
  "d" '(dap-hydra/body :which-key "dap"))

(hkk/ctrl-c
  ;; flycheck
  "!"  '(:ignore t :which-key "flycheck"))

(provide 'init-program)
;;; init-program.el ends here
