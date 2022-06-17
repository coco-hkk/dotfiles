;;; init-eaf.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:
;; refer to https://github.com/emacs-eaf/emacs-application-framework

;;; Code:

;;(add-to-list 'load-path "~/.emacs.d/site-lisp/eaf")

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
  (setq eaf-config-location "d:/emacs/.emacs.d/var/eaf"
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
