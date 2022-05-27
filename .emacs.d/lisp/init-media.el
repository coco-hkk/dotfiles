;;; init-media.el --- settings for media -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq default-process-coding-system '(utf-8-unix . chinese-gbk-dos))

(use-package emms
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

(hkk/ctrl-c
  "m"  '(:ignore t :which-key "media")

  "mb" '(emms-smart-browse :which-key "智能浏览")
  "mt" '(emms-add-directory-tree :which-key "音视频媒体库")
  "mh" '(emms-previous :which-key "上一首")
  "ml" '(emms-next :which-key "下一首")
  "mx" '(emms-toggle-repeat-playlist :which-key "切换重复播放列表")
  "my" '(emms-toggle-random-playlist :which-key "切换随机播放列表"))

(provide 'init-media)
;;; init-media.el ends here
