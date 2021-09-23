;;; init-startup.el --- Measure startup and require times -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
		     gcs-done)))

(provide 'init-startup)
;;; init-startup.el ends here
