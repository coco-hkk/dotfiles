;;; init-general.el --- setting for user interface, encoding, font-*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; User Interface configurations
(setq inhibit-startup-message t)                    ; 关闭启动页

(scroll-bar-mode -1)                                ; 禁掉滚动条
(tool-bar-mode -1)                                  ; 禁掉工具栏
(tooltip-mode -1)                                   ; 禁掉提示窗
(menu-bar-mode -1)                                  ; 禁掉菜单栏
(set-fringe-mode 5)                                 ; 设置侧边空白大小

(blink-cursor-mode -1)                              ; 禁止光标闪烁
(setq visible-bell t)                               ; 禁掉蜂鸣
(setq make-backup-files nil)                        ; 禁止备份文件
(setq create-lockfiles nil)                         ; 禁止创建 lockfile

(setq-default fill-column 80)                       ; 设置填充列
(global-display-fill-column-indicator-mode)         ; 设置 80 列显示

(setq-default tab-width 2)                          ; 设置 tab 宽度
(setq-default evil-shift-width tab-width)           ; 保持 shift 和 tab 宽度一致
(setq-default indent-tabs-mode nil)                 ; 设置空格替代 tab

(fset 'yes-or-no-p 'y-or-n-p)                       ; 使用 'y/n' 代替 'yes/no'

(setq word-wrap-by-category t)                      ; CJK wrap

(electric-pair-mode t)                              ; 自动补全括号

(column-number-mode)                                ; 在 Mode line 上显示列号
(setq display-line-numbers-type 'relative)          ; （可选）显示相对行号

;; 启用一些模式的行号
(dolist (mode '(prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; 禁掉一些模式的行号
(dolist (mode '(text-mode-hook
                org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq large-file-warning-threshold nil)             ; 不对大文件发出警告（如启动视频时）
(setq vc-follow-symlinks t)                         ; 不对符号链接发出警告
(setq ad-redefinition-action 'accept)               ; 为函数添加建议时不发出警告

;;; 编码设置
(set-language-environment 'utf-8)
(set-locale-environment "utf-8")
(set-default-coding-systems 'utf-8)

;; 状态栏显示
(size-indication-mode 1)                            ; 显示文件大小
(display-battery-mode 1)                            ; 显示电池状态

(setq display-time-format "%H:%M %a"
      display-time-default-load-average 2)
(display-time-mode 1)                               ; 显示时间

;; 设置窗体透明度，初始化全屏
(set-frame-parameter (selected-frame) 'alpha '(85 . 85))
(add-to-list 'default-frame-alist '(alpha . (85 . 85)))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; face sttribute
(set-face-attribute 'default        nil :font "ubuntu mono" :height 140)
(set-face-attribute 'fixed-pitch    nil :font "ubuntu mono" :height 1.0)
(set-face-attribute 'variable-pitch nil :font "ubuntu mono" :height 1.0 :weight 'regular)

;; 中英文字体设置
(defun set-font (english chinese english-size chinese-size)
  "Set CHINESE, ENGLISH font and size."
  (set-face-attribute 'default nil :font
                      (format "%s:pixelsize=%d"  english english-size))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family chinese :size chinese-size))))

(set-font "ubuntu mono" "ubuntu mono" 24 24)

(provide 'init-base)
;;; init-base.el ends here
