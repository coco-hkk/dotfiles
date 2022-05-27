;;; init-window.el --- settings for window -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq display-buffer-base-action
      '(display-buffer-reuse-mode-window
        display-buffer-reuse-window
        display-buffer-same-window))

;; If a popup does happen, don't resize windows to be equal-sized
(setq even-window-sizes nil)

;; 窗口间跳转
(use-package ace-window
  :bind
  ("M-o" . ace-window)
  :custom
  (aw-scope 'frame)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-minibuffer-flag t)
  :config
  (ace-window-display-mode 1))

;; 内置插件。窗口布局 undo/redo
;; 默认快捷键为 C-c 方向键左/右
(use-package winner
  :ensure nil
  :config
  (winner-mode))

(provide 'init-window)
;;; init-window.el ends here
