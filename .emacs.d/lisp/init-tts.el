;;; init-tts.el --- setting for user interface, encoding, font-*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package read-aloud
  :config
  (lax-plist-put read-aloud-engines "jampal"
  '(cmd "cscript"
		args ("d:/Jampal/ptts.vbs" "-r" "4")) )
  (setq read-aloud-engine "jampal"))

(provide 'init-tts)
;;; init-test.el ends here
