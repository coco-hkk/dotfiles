;;; init-package.el --- package management -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; gpg 签名设置
;;(setq package-check-signature nil)    ; 禁掉签名
;;(setq package-gnupghome-dir "/d/emacs/.emacs.d/elpa/gnupg")

(unless (featurep 'straight)
  ;; Bootstrap straight.el
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))

(setq straight-use-package-by-default t)
(setq straight-vc-git-default-clone-depth 1)
(setq straight-check-for-modifications '(watch-files find-when-checking))

;; Use straight.el for use-package expressions
(straight-use-package 'use-package)

;; Clean up unused repos with `straight-remove-unused-repos'

(provide 'init-package)
;;; init-package.el ends here
