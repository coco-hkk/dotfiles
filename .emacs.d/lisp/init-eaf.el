;;; init-eaf.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:
;; refer to https://github.com/emacs-eaf/emacs-application-framework

;;; Code:

(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")

(require 'eaf)

(require 'eaf-browser)
;(setq eaf-proxy-type "socks5")
;(setq eaf-proxy-host "127.0.0.1")
;(setq eaf-proxy-port "10800")
(setq browse-url-browser-function 'eaf-open-browser)
(defalias 'browse-web #'eaf-open-browser)

(require 'eaf-demo)

(require 'eaf-music-player)
(require 'eaf-video-player)

(require 'eaf-image-viewer)

(require 'eaf-pdf-viewer)
(setq eaf-pdf-dark-mode t)


(require 'eaf-org-previewer)
(require 'eaf-markdown-previewer)

;(defconst IS-MAC (eq system-type 'darwin))
;(defconst IS-WINDOWS (eq system-type 'windows-nt))
;
;(setq eaf-enable-debug t)
;
;(defun my-eaf-install-deps(app-dir)
;  "Install deps from dependencies.json.  APP-DIR app directory."
;  (let* ((deps-dict (with-temp-buffer
;                      (insert-file-contents (expand-file-name "dependencies.json" app-dir))
;                      (json-parse-string (buffer-string))))
;         (pip-deps (gethash "win32" (or (gethash "pip" deps-dict) (make-hash-table))))
;         (vue-install (gethash "vue_install" deps-dict))
;         (npm-install (gethash "npm_install" deps-dict))
;         (npm-rebuild (gethash "npm_rebuild" deps-dict))
;         (npm-cmd (if (memq system-type '(cygwin windows-nt ms-dos)) "npm.cmd" "npm")))
;    (when pip-deps
;      (dolist (pkg (append pip-deps nil))
;        (message "%s" (shell-command-to-string (format "pip install %s" pkg)))))
;    (when vue-install
;      (let ((default-directory app-dir))
;        (message "%s" (shell-command-to-string (format "%s install" npm-cmd)))
;        (message "%s" (shell-command-to-string (format "%s run build" npm-cmd)))))
;    (when npm-install
;      (let ((default-directory app-dir))
;        (message "%s" (shell-command-to-string (format "%s install" npm-cmd)))))
;    (when npm-rebuild
;      (let ((default-directory app-dir))
;        (message "%s" (shell-command-to-string (format "%s rebuild" npm-cmd)))))))
;
;
;(use-package ctable)
;(use-package epc)
;(use-package deferred)
;
;(use-package eaf
;  :straight (eaf :type git :host github :repo "emacs-eaf/emacs-application-framework"
;                 :files ("*")
;                 :post-build ("python" "install-eaf.py" "--install-core-deps"))
;  :config
;  (when IS-WINDOWS
;    (setq eaf-python-command "d:/python310/python.exe")
;    (setq eaf-wm-name "windows")))
;
;(use-package eaf-demo
;  :after (eaf)
;  :straight (eaf-demo :type git :host github :repo "emacs-eaf/eaf-demo" :files ("*")))
;
;(use-package eaf-browser
;  :after (eaf)
;  :straight (eaf-browser :type git :host github :repo "emacs-eaf/eaf-browser" :files ("*")
;                          :post-build (my-eaf-install-deps (straight--build-dir "eaf-browser"))))
;
;(use-package eaf-pdf-viewer
;  :after (eaf)
;  :straight (eaf-pdf-viewer :type git :host github :repo "emacs-eaf/eaf-pdf-viewer" :files ("*")
;                            :post-build (my-eaf-install-deps (straight--build-dir "eaf-pdf-viewer"))))
;
;(use-package eaf-video-player
;  :after (eaf)
;  :straight (eaf-video-player :type git :host github :repo "emacs-eaf/eaf-video-player" :files ("*")))
;
;(use-package eaf-music-player
;  :after (eaf)
;  :straight (eaf-music-player :type git :host github :repo "emacs-eaf/eaf-music-player" :files ("*")
;                            :post-build (my-eaf-install-deps (straight--build-dir "eaf-music-player"))))
;
;(use-package eaf-git
;  :after (eaf)
;  :straight (eaf-git
;             :type git :host github :repo "emacs-eaf/eaf-git" :files ("*")
;                            :post-build (my-eaf-install-deps (straight--build-dir "eaf-git"))))
;
;(use-package eaf-image-viewer
;  :after (eaf)
;  :straight (eaf-image-viewer
;             :type git :host github :repo "emacs-eaf/eaf-image-viewer" :files ("*")
;                            :post-build (my-eaf-install-deps (straight--build-dir "eaf-image-viewer"))))

;(use-package eaf-markdown-previewer
;  :after (eaf)
;  :straight (eaf-markdown-previewer :type git :host github :repo "emacs-eaf/eaf-markdown-previewer" :files ("*")
;                                    :post-build (my-eaf-install-deps (straight--build-dir "eaf-markdown-previewer"))))

(provide 'init-eaf)
;;; init-eaf.el ends here
