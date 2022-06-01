;;; init-tts.el --- setting for user interface, encoding, font-*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package read-aloud
  :config
  (lax-plist-put read-aloud-engines "jampal"
  '(cmd "cscript"
		args ("d:/Jampal/ptts.vbs" "-r" "4")) )
  (setq read-aloud-engine "jampal"))

(defhydra hydra-tts (:color teal
                            :exit t
                            :hint nil)
  "
              ^Jampal^
^^^^^^^^----------------------------------------
_b_: read buffer    _t_: read this
_s_: stop           _e_: change engines
"
  ("b" read-aloud-buf)
  ("t" read-aloud-this)
  ("s" read-aloud-stop :color red)
  ("e" read-aloud-change-engine))

(hkk/leader-key
  ;; org roam
  "j" '(hydra-tts/body :which-key "jampal"))

(provide 'init-tts)
;;; init-test.el ends here
