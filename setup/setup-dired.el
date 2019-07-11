;;; package --- summary
;;; Commentary:
;;; Code:

(use-package dired
  :ensure nil
  :config
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)

  (when sys/macp
    (setq dired-use-ls-dired nil)
    (when (executable-find "gls")
      (setq insert-directory-program "gls")))

  (when (or (and sys/macp (executable-find "gls"))
	    (and (not sys/macp) (executable-find "ls")))
    (setq ls-lisp-use-insert-directory-program t)
    (setq dired-listing-switches "-alh -group-directories-first")
    (use-package dired-quick-sort
      :ensure nil
      :init (dired-quick-sort-setup)))

  (use-package diredfl
    :ensure nil
    :init (diredfl-global-mode 1))

  (use-package dired-aux
    :ensure nil)
  
  (use-package dired-x
    :ensure nil
    :demand
    :config
    (let ((cmd (cond
		(sys/mac-x-p "open")
		(sys/linux-x-p "xdg-open")
		(sys/win32p "start")
		(t ""))))
      (setq dired-guess-shell-alist-user
	     `(("\\.pdf\\'" ,cmd)
              ("\\.docx\\'" ,cmd)
              ("\\.\\(?:djvu\\|eps\\)\\'" ,cmd)
              ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" ,cmd)
              ("\\.\\(?:xcf\\)\\'" ,cmd)
              ("\\.csv\\'" ,cmd)
              ("\\.tex\\'" ,cmd)
              ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'"
               ,cmd)
              ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)
              ("\\.html?\\'" ,cmd)
              ("\\.md\\'" ,cmd))))

    (setq dired-omit-files
	  (concat dired-omit-files
		  "\\|^.DS_Store$\\|^.projectile$\\|^.git*\\|^.svn$\\|^.vscode$\\|\\.js\\.meta$\\|\\.meta$\\|\\.elc$\\|^.emacs.*"))
    )
  )

(provide 'setup-dired)
;;; setup-dired.el ends here
