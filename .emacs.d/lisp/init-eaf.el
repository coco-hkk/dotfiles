;;; init-eaf.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:
;; refer to https://github.com/emacs-eaf/emacs-application-framework

;;; Code:

(use-package netease-cloud-music
  :defer 2
  :config
  (require 'netease-cloud-music-ui))

(defconst IS-WINDOWS (or (eq system-type 'ms-dos)
                         (eq system-type 'windows-nt)))

(defun my-eaf-install-deps (app-dir)
  "From APP-DIR install deps from dependencies.json."
  (let* ((deps-dict (with-temp-buffer
                      (insert-file-contents (expand-file-name "dependencies.json" app-dir))
                      (json-parse-string (buffer-string))))
         (pip-deps (gethash "win32" (or (gethash "pip" deps-dict) (make-hash-table))))
         (vue-install (gethash "vue_install" deps-dict))
         (npm-install (gethash "npm_install" deps-dict))
         (npm-rebuild (gethash "npm_rebuild" deps-dict))
         (npm-cmd (if (memq system-type '(cygwin windows-nt ms-dos)) "npm.cmd" "npm")))
    (when pip-deps
      (dolist (pkg (append pip-deps nil))
        (message "%s" (shell-command-to-string (format "pip install %s" pkg)))))
    (when vue-install
      (let ((default-directory app-dir))
        (message "%s" (shell-command-to-string (format "%s install" npm-cmd)))
        (message "%s" (shell-command-to-string (format "%s run build" npm-cmd)))))
    (when npm-install
      (let ((default-directory app-dir))
        (message "%s" (shell-command-to-string (format "%s install" npm-cmd)))))
    (when npm-rebuild
      (let ((default-directory app-dir))
        (message "%s" (shell-command-to-string (format "%s rebuild" npm-cmd)))))))

(use-package eaf
  :straight (eaf :type git
                 :host github
                 :repo "emacs-eaf/emacs-application-framework"
                 :files ("*")
                 :post-build ("python" "install-eaf.py" "--install-core-deps"))
  :init
  (setq eaf-config-location "d:/emacs/.emacs.d/var/eaf/"
        eaf-browser-default-search-engine "bing"
        eaf-enable-debug t)
  :config
  (when IS-WINDOWS
    (setq eaf-python-command "d:/Python310/python.exe")
    (setq eaf-wm-name "windows")))

(use-package eaf-demo
  :after (eaf)
  :straight (eaf-demo :type git
                      :host github
                      :repo "emacs-eaf/eaf-demo"
                      :files ("*")))

(use-package eaf-browser
  :after (eaf)
  :straight (eaf-browser :type git
                         :host github
                         :repo "emacs-eaf/eaf-browser"
                         :files ("*")
                         :post-build (my-eaf-install-deps (straight--build-dir "eaf-browser"))))

(use-package eaf-video-player
  :after (eaf)
  :straight (eaf-video-player :type git
                              :host github
                              :repo "emacs-eaf/eaf-video-player"
                              :files ("*")))

(use-package eaf-pdf-viewer
  :after (eaf)
  :straight (eaf-pdf-viewer :type git
                            :host github
                            :repo "emacs-eaf/eaf-pdf-viewer"
                            :files ("*")
                            :post-build (my-eaf-install-deps (straight--build-dir "eaf-pdf-viewer"))))

;;(use-package eaf
;;  :straight nil
;;  :load-path "site-lisp/eaf"
;;  :init
;;  (setq eaf-python-command "d:/Python310/python.exe"
;;        eaf-enable-debug t
;;        eaf-pdf-dark-mode t
;;        eaf-browser-default-search-engine "bing"
;;        eaf-browser-blank-page-url "https://www.baidu.com"
;;        eaf-config-location "d:/emacs/.emacs.d/etc/eaf/"
;;
;;        ;;eaf-proxy-type "socks5"
;;        ;;eaf-proxy-host "127.0.0.1"
;;        ;;eaf-proxy-port "10800"
;;        )
;;  :config
;;  ;;(require 'eaf-browser)
;;  ;;(require 'eaf-image-viewer)
;;  (require 'eaf-video-player)
;;  ;;(require 'eaf-music-player)
;;  ;;(require 'eaf-pdf-viewer)
;;  ;;(require 'eaf-mindmap)
;;  ;;(require 'eaf-netease-cloud-music)
;;  ;;(require 'eaf-org-previewer)
;;  ;;(require 'eaf-markdown-previewer)
;;  )

(provide 'init-eaf)
;;; init-eaf.el ends here
