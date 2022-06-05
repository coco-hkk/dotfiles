;;; init-eaf.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:
;; refer to https://github.com/emacs-eaf/emacs-application-framework

;;; Code:

(add-to-list 'load-path "~/.emacs.d/site-lisp/eaf")

(require 'eaf)

(setq eaf-enable-debug t)

(require 'eaf-browser)
;;(setq eaf-proxy-type "socks5")
;;(setq eaf-proxy-host "127.0.0.1")
;;(setq eaf-proxy-port "10800")
(setq browse-url-browser-function 'eaf-open-browser)
(defalias 'browse-web #'eaf-open-browser)

(require 'eaf-image-viewer)
(require 'eaf-music-player)
(require 'eaf-video-player)

(require 'eaf-file-manager)

(require 'eaf-mindmap)

(use-package netease-cloud-music
  :config
  (require 'netease-cloud-music-ui))

(require 'eaf-netease-cloud-music)

(require 'eaf-pdf-viewer)
(setq eaf-pdf-dark-mode t)

(require 'eaf-org-previewer)
(require 'eaf-markdown-previewer)

(defhydra hydra-eaf (
                     :color pink
                     :exit t
                     :hint nil)
  "
^browse^        ^mindmap^       ^File Manager^
^^^^^^^^-------------------------------------------------
_b_: browse     _m_: create
_B_: history    _M_: open
  ^ ^             ^ ^
  ^ ^             ^ ^              ^ ^
"
  ("b" eaf-open-browser)
  ("B" eaf-open-browser-with-history)

  ("m" eaf-create-mindmap)
  ("M" eaf-open-mindmap)

  ("q" nil "quit" :color red))

(hkk/leader-key
  ;; org roam
  "e" '(hydra-eaf/body :which-key "eaf"))

(provide 'init-eaf)
;;; init-eaf.el ends here
