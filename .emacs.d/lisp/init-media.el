;;; init-media.el --- settings for media -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package emms
  :defer t
  :init
  (setq default-process-coding-system '(utf-8-unix . chinese-gbk-dos))
  :custom
  ;; 音视频媒体库
  (emms-source-file-default-directory "f:/多媒体")

  ;; 播放列表
  (emms-playlist-buffer-name "*播放列表*")        

  ;; 播放列表循环播放
  (emms-repeat-playlist t)                         

  ;; 交互式播放
  (emms-playlist-default-major-mode 'emms-playlist-mode)

  ;; 播放器列表
  (emms-player-list '(emms-player-mpv
                      emms-player-mplayer))

  ;; Reading metadata, 可用 Tinytag 和 Exiftool。
  ;; pip install tinytag
  (emms-info-functions '(emms-info-tinytag))

  ;; Load cover images，使用 ImageMagick
  (emms-browser-covers 'emms-browser-cache-thumbnail-async)
  
  (emms-browser-make-filter "all" 'ignore)

  :config
  (require 'emms-setup)
  (emms-all)
  
  (emms-lyrics 1)
  
  (defvar emms-browser-info-title-format "%i%n")
  (defvar emms-browser-playlist-info-title-format
    emms-browser-info-title-format)

  (define-emms-simple-player mpv '(file url)
    (regexp-opt '(".ogg" ".mp3" ".wav" ".mpg" ".mpeg" ".wmv" ".wma"
                  ".mov" ".avi" ".divx" ".ogm" ".asf" ".mkv" "http://" "mms://"
                  ".rm" ".rmvb" ".mp4" ".flac" ".vob" ".m4a" ".flv" ".ogv" ".pls"))
    "mpv" "--no-terminal" " --fullscreen" "--quiet" "--really-quiet")

  (define-emms-simple-player mplayer '(file url)
    (regexp-opt '(".ogg" ".mp3" ".wav" ".mpg" ".mpeg" ".wmv" ".wma"
                  ".mov" ".avi" ".divx" ".ogm" ".asf" ".mkv" "http://" "mms://"
                  ".rm" ".rmvb" ".mp4" ".flac" ".vob" ".m4a" ".flv" ".ogv" ".pls"))
    "mplayer" "-slave" "-quiet" "-really-quiet" "-fullscreen"))

(provide 'init-media)
;;; init-media.el ends here
