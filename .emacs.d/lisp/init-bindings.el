;;; init-bindings.el --- settings for keybindings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; ESC Cancels All
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; 快捷键提示包
(use-package which-key
  :hook (after-init . which-key-mode)
  :custom
  (which-key-idle-delay 0.4))

(use-package general
  :demand t
  :config
  (general-evil-setup t)
  (general-create-definer hkk/leader-key
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (general-create-definer hkk/ctrl-c
    :prefix "C-c"))

(use-package hydra
  :after general)

(defhydra hydra-options (:color pink
                                :exit t
                                :hint nil)
  "
   ^Emms^             ^Jampal^                ^File^           ^Fanyi^
--------------------------------------------------------------------------------
_el_: 播放列表    _je_: change engines    _ff_: init.el     _yd_: dwim
_ed_: 媒体库      _jr_: read buffer       _fl_: lisp        _ye_: dwim2
_ej_: 上一首      _js_: stop              _fe_: .eamcs.d    _yh_: history
_ek_: 下一首      _jt_: read this            ^ ^            _yq_: copy&query
_er_: 列表重播
_es_: 列表随机

   ^benchmark^        ^Others^
--------------------------------------------------------------------------------
_bt_: show tree     _s_: toggle eshell
_bb_: show table    _w_: whitespace
                    _t_: themes

"
  ("bb" benchmark-init/show-durations-tabulated)
  ("bt" benchmark-init/show-durations-tree)

  ("el" emms-playlist-mode-switch-buffer)
  ("ed" emms-add-directory-tree)
  ("ej" emms-previous)
  ("ek" emms-next)
  ("er" emms-toggle-repeat-playlist)
  ("es" emms-toggle-random-playlist)

  ("je" read-aloud-change-engine)
  ("jr" read-aloud-buf)
  ("jt" read-aloud-this)
  ("js" read-aloud-stop)

  ("ff" (lambda () (interactive) (find-file (expand-file-name "~/.emacs.d/init.el"))))
  ("fl" (lambda () (interactive) (dired (expand-file-name "~/.emacs.d/lisp/"))))
  ("fe" (lambda () (interactive) (dired (expand-file-name "~/.emacs.d/"))))

  ("yd" fanyi-dwim)
  ("ye" fanyi-dwim2)
  ("yh" fanyi-from-history)
  ("yq" fanyi-copy-query-word)

  ("s" eshell-toggle)
  ("w" whitespace-mode)
  ("t" counsel-load-theme)

  ("q" nil "quit" :color pink))

(hkk/leader-key
  ;; hydra keybindings
  "a" '(hydra-options/body :which-key "options"))

(provide 'init-bindings)
;;; init-bindings.el ends here
