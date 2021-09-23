;;; init-media.el --- settings for media -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq default-process-coding-system '(utf-8-unix . chinese-gbk-dos))

(use-package emms
  :commands emms
  :config
  (require 'emms-setup)
  (require 'emms-player-mplayer)
  (emms-all)

  (setq emms-player-list '(emms-player-mplayer))

  (setq emms-repeat-playlist nil
        emms-playlist-buffer-name "*Emms*"
        emms-source-file-default-directory "F:\音乐")

  (define-emms-simple-player mplayer '(file url)
    (regexp-opt '(".ogg" ".mp3" ".wav" ".mpg" ".mpeg" ".wmv" ".wma"
                  ".mov" ".avi" ".divx" ".ogm" ".asf" ".mkv" "http://" "mms://"
                  ".rm" ".rmvb" ".mp4" ".flac" ".vob" ".m4a" ".flv" ".ogv" ".pls"))
    "mplayer" "-slave" "-quiet" "-really-quiet" "-fullscreen")

  (es/leader-key-def
    "m"  '(:ignore t :which-key "media")
    "mp" '(emms-pause :which-key "play / pause")
    "mf" '(emms-play-file :which-key "play file")))

(provide 'init-media)
 ;;; init-media.el ends here
