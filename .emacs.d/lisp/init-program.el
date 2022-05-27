;; init-program.el --- settings for magit -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package web-mode
  :mode "(\\.\\(html?\\|ejs\\|tsx\\|jsx\\)\\'"
  :config
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-attribute-indent-offset 2))

;; 1. Start the server with `httpd-start'
;; 2. Use `impatient-mode' on any buffer
(use-package impatient-mode)
(use-package skewer-mode)

(provide 'init-program)
;;; init-program.el ends here
