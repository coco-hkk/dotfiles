;; init-terminal.el --- settings for terminal -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(add-to-list 'load-path "d:/CMake/bin")

(use-package vterm
  :commands (vterm))

(use-package multi-vterm
  :commands (multi-vterm)
  :general
  (:keymaps 'vterm-mode-map
            "C-h" 'vterm-send-C-h
            "RET" 'vterm-send-return
            ))

(provide 'init-terminal)
;;; init-terminal.el ends here
