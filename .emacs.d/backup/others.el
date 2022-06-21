;;; 内置功能，在 init-builtin.el 中重新配置
(global-auto-revert-mode t)              ; 文件在硬盘改变后及时刷新 Buffer
(save-place-mode 1)                      ; 保存上次关闭文件时光标位置
(global-display-line-numbers-mode t)     ; 全局行号设置
(delete-selection-mode t)                ; 编辑前先自动删除选中区域内容

;; 代理设置
 (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")             ; 不加可能有问题
 (setq url-proxy-services
       '(("no_proxy" . "^\\(192\\.168\\..*\\)")
         ("http" . "127.0.0.1:10800")
         ("https" . "127.0.0.1:10800")))

;;(setq make-backup-files nil)                        ; 禁止备份文件
(setq backup-directory-alist '(("." . (no-littering-expand-var-file-name "backup/")))
      auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save-list/") t)))

