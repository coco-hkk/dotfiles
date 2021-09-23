;;; init-dired.el --- settings for dired -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package all-the-icons-dired
	:hook (dired-mode . all-the-icons-dired-mode))

(use-package dired
	:ensure nil
	:defer 1
	:commands (dired dired-jump)
	:bind (("C-x C-j" . dired-jump))
	:config
	(setq dired-listing-switches "-lah"
				dired-omit-files "^\\.[^.].*"
				dired-omit-verbose nil
				dired-hide-details-hide-symlink-targets nil
				delete-by-moving-to-trash t)

	(autoload 'dired-omit-mode "dired-x")

	(add-hook 'dired-load-hook
						(lambda ()
							(interactive)
							(dired-collapse)))

	(add-hook 'dired-mode-hook
						(lambda ()
							(interactive)
							(dired-omit-mode 1)
							(dired-hide-details-mode 1)
							(hl-line-mode 1)))

	(use-package dired-rainbow
		:defer 2
		:config
		(dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
		(dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
		(dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
		(dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
		(dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
		(dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
		(dired-rainbow-define media "#de751f" ("mp3" "mp4" "mkv" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
		(dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
		(dired-rainbow-define log "#c17d11" ("log"))
		(dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
		(dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
		(dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
		(dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
		(dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
		(dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
		(dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
		(dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
		(dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
		(dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
		(dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*"))

	(use-package dired-single
		:defer t)

	(use-package dired-ranger
		:defer t)

	(use-package dired-collapse
		:defer t)

	(evil-collection-define-key 'normal 'dired-mode-map
		"h" 'dired-single-up-directory
    "H" 'dired-omit-mode
		"l" 'dired-single-buffer
		"y" 'dired-ranger-copy
		"X" 'dired-ranger-move
		"p" 'dired-ranger-paste))

(use-package openwith
  :config
  (setq openwith-associations
        (list
          (list (openwith-make-extension-regexp
                '("mpg" "mpeg" "mp3" "mp4"
                  "avi" "wmv" "wav" "mov" "flv"
                  "ogm" "ogg" "mkv"))
                "mpv"
                '(file))
          (list (openwith-make-extension-regexp
                '("xbm" "pbm" "pgm" "ppm" "pnm"
                  "png" "gif" "bmp" "tif" "jpeg")) ;; Removed jpg because Telega was
                  ;; causing feh to be opened...
                  "feh"
                  '(file))
          (list (openwith-make-extension-regexp
                '("pdf"))
                "zathura"
                '(file)))))

(provide 'init-dired)
;;; init-dired.el ends here
