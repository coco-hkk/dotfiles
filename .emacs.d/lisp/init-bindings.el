;;; init-bindings.el --- settings for keybindings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; ESC Cancels All
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; 快捷键提示包
(use-package which-key
  :init (which-key-mode)
  :custom
  (which-key-idle-delay 0.4))

(use-package general
  :init (general-evil-setup t)
  :config
  (general-create-definer hkk/leader-key
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (general-create-definer hkk/ctrl-c
    :prefix "C-c"))

(use-package hydra
  :after (general))

(defhydra hydra-options (:color pink
                                :exit t
                                :hint nil)
  "
^Media^             ^File^           ^Fanyi^          ^Others
^^^^^^^-----------------------------------------------------------------
_a_: 播放列表    _f_: init.el      _D_: dwim         _s_: toggle eshell
_t_: 媒体库      _d_: lisp         _E_: dwim2        _w_: whitespace
_h_: 上一首      _e_: .eamcs.d     _F_: history      _T_: themes
_l_: 下一首      ^ ^               _g_: copy&query
_x_: 列表重播
_y_: 列表随机
"
  ("a" emms-playlist-mode-switch-buffer)
  ("t" emms-add-directory-tree)
  ("h" emms-previous)
  ("l" emms-next)
  ("x" emms-toggle-repeat-playlist)
  ("y" emms-toggle-random-playlist)

  ("f" (lambda () (interactive) (find-file (expand-file-name "~/.emacs.d/init.el"))))
  ("d" (lambda () (interactive) (dired (expand-file-name "~/.emacs.d/lisp/"))))
  ("e" (lambda () (interactive) (dired (expand-file-name "~/.emacs.d/"))))

  ("D" fanyi-dwim)
  ("E" fanyi-dwim2)
  ("F" fanyi-from-history)
  ("g" fanyi-copy-query-word)

  ("s" eshell-toggle)
  ("w" whitespace-mode)
  ("T" counsel-load-theme))

(hkk/leader-key
  ;; hydra keybindings
  "a" '(hydra-options/body :which-key "options"))

(provide 'init-bindings)
;;; init-bindings.el ends here
