;;; init-eaf.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:
;; refer to https://github.com/emacs-eaf/emacs-application-framework

;;; Code:

;;(add-to-list 'load-path "~/.emacs.d/site-lisp/eaf")

(use-package netease-cloud-music
  :defer 2
  :config
  (require 'netease-cloud-music-ui))

(use-package eaf
  :straight nil
  :load-path "site-lisp/eaf"
  :defer 2
  :init
  (setq eaf-enable-debug t
        ;;eaf-proxy-type "socks5"
        ;;eaf-proxy-host "127.0.0.1"
        ;;eaf-proxy-port "10800"
        )
  :config
  (require 'eaf-browser)

  (require 'eaf-image-viewer)
  (require 'eaf-music-player)
  (require 'eaf-video-player)

  (require 'eaf-file-manager)

  (require 'eaf-mindmap)

  (require 'eaf-netease-cloud-music)

  (require 'eaf-pdf-viewer)
  (setq eaf-pdf-dark-mode t)

  (require 'eaf-org-previewer)
  (require 'eaf-markdown-previewer))

(defhydra hydra-eaf (:color pink
                            :exit t
                            :hint nil)
  "
^browse^        ^mindmap^       ^File Manager^      ^open^
^^^^^^^^-------------------------------------------------------
_b_: browse     _m_: create     _f_: manager      _o_: open
_s_: search     _M_: open
  ^ ^             ^ ^
  ^ ^             ^ ^              ^ ^
"
  ("b" eaf-open-browser)
  ("s" eaf-open-browser-with-history)

  ("m" eaf-create-mindmap)
  ("M" eaf-open-mindmap)

  ("f" eaf-open-file-manager)

  ("o" eaf-open)

  ("q" nil "quit" :color red))

(hkk/leader-key
  ;; org roam
  "e" '(hydra-eaf/body :which-key "eaf"))

(provide 'init-eaf)
;;; init-eaf.el ends here
