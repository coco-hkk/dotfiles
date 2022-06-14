;;; init-package.el --- package management -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; gpg 签名设置
;;(setq package-check-signature nil)    ; 禁掉签名
;;(setq package-gnupghome-dir "/d/emacs/.emacs.d/elpa/gnupg")

;; Clean up unused repos with `straight-remove-unused-repos'
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

(setq straight-use-package-by-default t
      straight-vc-git-default-clone-depth 1)

;; Use straight.el for use-package expressions
(straight-use-package 'use-package)

;;; 启动时间统计
(use-package benchmark-init
  :config
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; 保持 .emacs.d 清洁
(use-package no-littering
  :config
  (with-eval-after-load 'recentf)
  (set 'recentf-exclude
       '(no-littering-var-directory
         no-littering-etc-directory
         (expand-file-name "elpa" user-emacs-directory)))

  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(provide 'init-package)
;;; init-package.el ends here
