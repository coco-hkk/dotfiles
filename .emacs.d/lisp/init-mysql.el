;; init-mysql.el --- settings for mysql -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq sql-mysql-options '("-C" "-f" "-t" "-n")) ; for windows
(setq sql-user "hkk")
(setq sql-password "")

(setq sql-server "localhost")

(provide 'init-mysql)
;;; init-mysql.el ends here
