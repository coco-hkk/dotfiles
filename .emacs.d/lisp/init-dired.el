;;; init-dired.el --- settings for dired -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package all-the-icons-dired
	:hook (dired-mode . all-the-icons-dired-mode))

;; Use ( to toggle dired-hide-details-mode
(use-package dired
  :straight nil
  :bind (("C-x C-j" . dired-jump)
         :map dired-mode-map
         ;; consistent with ivy
         ("C-c C-e"   . wdired-change-to-wdired-mode))
  :custom
  (dired-dwim-target t)
  (dired-bind-vm nil)
  (dired-bind-man nil)
  (dired-bind-info nil)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-auto-revert-buffer #'dired-directory-changed-p)
  (dired-hide-details-hide-symlink-targets nil)
  (dired-listing-switches "-AFhlv")
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
  "H" 'dired-omit-mode))

(use-package dired-aux
  :straight nil
  :after dired
  :config
  (with-no-warnings
    (defvar dired-dotfiles-show t)
    (defun dired-dotfiles-toggle (&rest _)
      "Show/hide dotfiles."
      (interactive)
      (if (not dired-dotfiles-show)
          (revert-buffer)
        (dired-mark-files-regexp "^\\.")
        (dired-do-kill-lines))
      (setq-local dired-dotfiles-show (not dired-dotfiles-show)))

    (advice-add 'dired-do-print :override #'dired-dotfiles-toggle))
  :custom
  (dired-vc-rename-file t)
  (dired-do-revert-buffer t)
  (dired-isearch-filenames 'dwim)
  (dired-create-destination-dirs 'ask))

(use-package dired-x
  :straight nil
  :hook (dired-mode . dired-omit-mode)
  :custom
  (dired-omit-verbose nil)
  (dired-omit-files (rx string-start
                        (or ".cache")
                        string-end))
  ;; Dont prompt about killing buffer visiting delete file
  (dired-clean-confirm-killing-deleted-buffers nil)
  (dired-guess-shell-alist-user `((,(rx "."
                                        (or
                                         ;; Videos
                                         "mp4" "avi" "mkv" "flv" "ogv" "ogg" "mov"
                                         ;; Music
                                         "wav" "mp3" "flac"
                                         ;; Images
                                         "jpg" "jpeg" "png" "gif" "xpm" "svg" "bmp"
                                         ;; Docs
                                         "pdf" "md" "djvu" "ps" "eps" "doc" "docx" "xls" "xlsx" "ppt" "pptx")
                                        string-end)
                                   ,(pcase system-type
                                      ('gnu/linux "xdg-open")
                                      ('darwin "open")
                                      ('windows-nt "start")
                                      (_ ""))))))

;; Make dired colorful
(use-package diredfl
  :hook (dired-mode . diredfl-mode))

;; Make dired narrow-able
(use-package dired-narrow
  :after dired)

;; Show subtree in dired
(use-package dired-subtree
  :after dired
  :custom
  (dired-subtree-use-backgrounds nil))

;; Keep ~/.emacs.d clean
(use-package no-littering)

(provide 'init-dired)
;;; init-dired.el ends here
