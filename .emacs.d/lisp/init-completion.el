;;; init-completion.el --- settings for completion -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Advanced completion style
(use-package orderless
  :config
  (orderless-define-completion-style orderless+new
    (orderless-matching-styles '(orderless-initialism
                                 orderless-literal
                                 orderless-regexp)))

  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        orderless-component-separator #'orderless-escapable-split-on-space
        completion-category-overrides '((file (styles orderless+new))
                                        (command  (styles orderless+new))
                                        (symbol   (styles orderless+new))
                                        (variable (styles orderless+new))
                                        )))

;;; completion, such as company
(use-package corfu
  :straight '(corfu :files (:defaults "extensions/*"))
  :defer 5
  :bind (:map corfu-map
              ("SPC" . corfu-insert-separator)
              ("TAB" . corfu-insert))
  :config
  ;; 自动补全
  (setq corfu-auto t
        ;; 补全循环
        corfu-cycle t
        ;; minibuffer 显示帮助文档
        corfu-echo-documentation t)

  ;; (global-corfu-mode t)
  (corfu-history-mode t)
  (corfu-indexed-mode))

;; completion document
(use-package corfu-doc
  :hook (corfu-mode . corfu-doc-mode)
  :config
  (corfu-doc--popup-show)
  (setq corfu-doc-display-within-parent-frame nil
        corfu-doc--frame t))

;; Add extensions
(use-package cape
  :after corfu
  :config
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

;; Example configuration for Consult
(use-package consult
  :bind (("C-x b"   . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)

         ("M-y" . consult-yank-pop)
         ("<help> a" . consult-apropos)
         :map minibuffer-local-map
         ("C-s" . consult-history))

  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  (advice-add #'register-preview :override #'consult-register-window)

  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-theme :preview-key nil
   consult-ripgrep
   consult-git-grep
   consult-grep
   consult-bookmark
   consult-recent-file
   consult-xref
   consult--source-bookmark
   consult--source-recent-file
   consult--source-project-recent-file
   :preview-key (kbd "M-.")))

;;; minibuffer completion
(use-package vertico
  :straight (vertico :files (:defaults "extensions/*"))
  :hook (after-init . vertico-mode)
  :custom-face
  (vertico-current ((t (:background "#3a3f5a"))))
  :config
  (vertico-mode)
  (vertico-multiform-mode)
  (vertico-indexed-mode)
  (setq vertico-count 10)
  )

(use-package vertico-posframe
  :after vertico
  :init
  (setq vertico-posframe-parameters
        '((left-fringe . 8)
          (right-fringe . 8)))
  :config
  (vertico-posframe-mode 1))

;; Rich annotations in the minibuffer
(use-package marginalia
  :after vertico
  :config
  (marginalia-mode)
  (marginalia--ellipsis)
  (marginalia--minibuffer-setup))

;; minibuffer completion icon
(use-package all-the-icons-completion
  :hook
  (after-init . all-the-icons-completion-mode)
  (marginalia-mode . all-the-icons-completion-marginalia-setup))

;; embark
(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("M-." . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)  ;; alternative for `describe-bindings'
   ))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :after (:all embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(provide 'init-completion)
;;; init-completion.el ends here
