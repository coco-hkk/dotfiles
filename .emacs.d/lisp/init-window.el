;;; init-window.el --- settings for window -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; 窗口间跳转
(use-package ace-window
  :bind
  ("M-o" . ace-window)
  :config
  (setq aw-scope 'frame
        aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-minibuffer-flag t)

  (ace-window-display-mode 1))

;; 内置插件。窗口布局 undo/redo
;; 默认快捷键为 C-c 方向键左/右
(use-package winner
  :straight (:type built-in)
  :hook (after-init . winner-mode))

(provide 'init-window)
;;; init-window.el ends here
