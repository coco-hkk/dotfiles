;;; init-general.el --- setting for user interface, encoding, font-*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; User Interface configurations
(setq inhibit-startup-message t)    ; 关闭启动页

(scroll-bar-mode -1)                ; 禁掉滚动条
(tool-bar-mode -1)                  ; 禁掉工具栏
(tooltip-mode -1)                   ; 禁掉提示窗
(menu-bar-mode -1)                  ; 禁掉菜单栏
(set-fringe-mode 5)                 ; 设置侧边空白大小

(blink-cursor-mode -1)              ; 去掉光标闪烁

(setq visible-bell t)               ; 禁掉蜂鸣
(setq make-backup-files nil)        ; 禁止备份文件

(fset 'yes-or-no-p 'y-or-n-p)       ; 使用 'y/n' 代替 'yes/no'

(setq-default fill-column 80)       ; 设置填充列
(global-display-fill-column-indicator-mode) ; 80列显示

(setq word-wrap-by-category t)      ; CJK wrap

(electric-pair-mode t)              ; 自动补全括号

;; tab width setting
(setq-default tab-width 2)
(setq-default evil-shift-width tab-width)

(setq-default indent-tabs-mode nil) ; 空格替代 tab

;; 设置窗体透明度及全屏
(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; 开启行号
(column-number-mode)

;; Enable line numbers for some modes
;; global setting: (global-display-line-numbers-mode t)
(dolist (mode '(prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; 禁掉一些模式的行号
(dolist (mode '(text-mode-hook
                org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq large-file-warning-threshold nil)    ; Don’t warn for large files (shows up when launching videos)
(setq vc-follow-symlinks t)                ; Don’t warn for symlinked files
(setq ad-redefinition-action 'accept)      ; Don’t warn when advice is added for functions

;;; 编码设置
(set-language-environment 'utf-8)
(set-locale-environment "utf-8")
(set-default-coding-systems 'utf-8)

;;; 中英文等宽字体
(defun set-font (english chinese english-size chinese-size)
  "Set CHINESE and ENGLISH font size."
   (set-face-attribute 'default nil :font
		       (format   "%s:pixelsize=%d"  english english-size))
   (dolist (charset '(kana han symbol cjk-misc bopomofo))
     (set-fontset-font (frame-parameter nil 'font) charset
		       (font-spec :family chinese :size chinese-size))))

;;(set-font   "Dejavu Sans Mono" "WenQuanYi Zen Hei Mono" 16 20)

(set-face-attribute 'default nil :font "ubuntu mono" :height 140)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "ubuntu mono" :height 1.0)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "ubuntu mono" :height 1.0 :weight 'regular)

(provide 'init-base)
;;; init-base.el ends here
