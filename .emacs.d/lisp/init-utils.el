;; init-utils.el --- settings for utils -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package command-log-mode
  :commands command-log-mode)

(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(provide 'init-utils)
;;; init-utils.el ends here
