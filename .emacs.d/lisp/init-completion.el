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

  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((command  (styles orderless+new))
                                        (symbol   (styles orderless+new))
                                        (variable (styles orderless+new)))))

;;; completion, such as company
(use-package corfu
  :straight '(corfu :files (:defaults "extensions/*"))
  :defer 5
  :bind (:map corfu-map
              ("SPC" . corfu-insert-separator)
              ("C-j" . corfu-next)
              ("C-k" . corfu-previous)
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
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b"   . consult-buffer)              ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-s bindings (search-map)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
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
   consult-ripgrep
   consult-git-grep
   consult-grep
   consult-bookmark
   consult-recent-file
   consult-xref
   consult--source-bookmark
   consult--source-recent-file
   consult--source-project-recent-file
   :preview-key (kbd "M-."))
  )

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

  (bind-keys :map vertico-map
             ("C-j" . vertico-next)
             ("C-k" . vertico-previous)))

;; Rich annotations in the minibuffer
(use-package marginalia
  :after vertico
  :config
  (marginalia-mode)
  (marginalia--ellipsis)
  (marginalia--minibuffer-setup)
  (marginalia-classify-original-category)
  (setq marginalia--command t))

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
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  )

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :after (:all embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(provide 'init-completion)
;;; init-completion.el ends here
