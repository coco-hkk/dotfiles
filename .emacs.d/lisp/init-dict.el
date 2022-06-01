;;; init-dict.el --- settings for yasnippet -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package fanyi
  :ensure t
  :custom
  (fanyi-sound-player-support-https t)
  (fanyi-providers '(;; 海词
                     fanyi-haici-provider
                     ;; 有道同义词词典
                     fanyi-youdao-thesaurus-provider
                     ;; Etymonline
                     fanyi-etymon-provider
                     ;; Longman
                     fanyi-longman-provider
                     ;; LibreTranslate
                     fanyi-libre-provider)))

(provide 'init-dict)
;;; init-dict.el ends here
