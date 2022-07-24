;;; ask-dired.el --- Files, Dired and sidetree -*- lexical-binding: t -*-

;; Copyright (C) 2021 cgfork

;; Author: cgfork(cg.fork@gmail.com)
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

;; Installation
(unless ask-start-emacs-without-packages
  (use-package dired-sidebar)
  (when (display-graphic-p)
    (use-package all-the-icons)
    (use-package all-the-icons-dired)))

;; Define

(defun ask-open-file-in-external-app (&optional file)
  "Open the current file or dired marked files in external app.

If the FILF is provided, open it."
  (interactive)
  (let* ((filelist (if file
		      (progn (list file))
		    (if (string-equal major-mode "dired-mode")
			(dired-get-marked-files)
		      (list (buffer-file-name))))))
      (cond
       ((eq system-type 'windows-nt)
	(mapc
	 (lambda (filename)
	   (when (y-or-n-p (format "Open %s? " filename) filename)
	     (shell-command (concat "PowerShell -Command \"Invoke-Item -LiteralPath\" " "'" (shell-quote-argument (expand-file-name filename)) "'"))))
	 filelist))
       ((eq system-type 'darwin)
	(mapc
	 (lambda (filename)
	   (when (y-or-n-p (format "Open %s? " filename))
	     (shell-command (concat "open " (shell-quote-argument filename)))))
	 filelist))
       ((eq system-type 'gnu/linux)
	(mapc
	 (lambda (filename)
	   (when (y-or-n-p (format "Open %s? " filename))
	     (let ((process-connection-type nil))
	       (start-process "" nil "xdg-open" filename))))
	 filelist)))))

(defun ask-open-file-in-place (beg end &optional external)
  "Open the file which is the active selection.

If the EXTERNAL is provided, open it in external app."
  (interactive (if (region-active-p)
		   (list (region-beginning) (region-end) (when current-prefix-arg t))
		 (list (point-min) (point-max) (when current-prefix-arg))))
  (let* ((filename (buffer-substring beg end))
	 (openit (y-or-n-p (format "Open %s? " filename))))
    (when openit
      (if external
	  (ask-open-file-in-external-app filename)
	(find-file filename)))))

(defun ask-show-file-in-desktop (file)
  "Show the given FILE in desktop (Mac Finder, File Explorer, Linux File Manager)."
  (cond
   ((eq system-type 'windows-nt)
    (shell-command (format "PowerShell -Command Start-Process Explorer -FilePath %s" (shell-quote-argument file))))
   ((eq system-type 'darwin)
    (shell-command
     (concat "open -R " (shell-quote-argument file))))
   ((eq system-type 'gnu/linux)
    (let ((process-connection-type nil)
	  (open-file-program (if (file-exists-p "/usr/bin/gvfs-open")
				 "/usr/bin/gvfs-open"
			       "/usr/bin/xdg-open")))
      (start-process "" nil open-file-program (shell-quote-argument file))))))

(defun ask-show-current-file-in-desktop ()
  "Show the current file in desktop."
  (interactive)
  (let ((filename (if (buffer-file-name) (buffer-file-name) default-directory)))
    (ask-show-file-in-desktop (expand-file-name filename))))

(defun ask-show-selected-file-in-desktop (beg end)
  "Show the selected file in desktop."
   (interactive (if (region-active-p)
		   (list (region-beginning) (region-end) (when current-prefix-arg t))
		  (list (point-min) (point-max) (when current-prefix-arg))))
   (let* ((filename (buffer-substring beg end)))
     (ask-show-file-in-desktop (expand-file-name filename))))

(defun ask-open-file-in-vscode (file)
  "Open the given FILE in vscode."
  (cond
   ((eq system-type 'window-nt)
    (shell-command (format "Code \"%s\"" file)))
   ((eq system-type 'darwin)
    (shell-command (format "open -a Visual\\ Studio\\ Code.app \"%s\"" file)))
   ((eq system-type 'gnu/linux)
    (shell-command (format "code \"%s\"" file)))))

(defun ask-open-current-file-in-vscode ()
  "Open the current file in vscode."
  (interactive)
  (let ((filename (if (buffer-file-name) (buffer-file-name) default-directory)))
    (ask-open-file-in-vscode (expand-file-name filename))))

(defun ask-open-selected-file-in-vscode (beg end)
  "Open the selected file in vscode."
   (interactive (if (region-active-p)
		   (list (region-beginning) (region-end) (when current-prefix-arg t))
		  (list (point-min) (point-max) (when current-prefix-arg))))
   (let* ((filename (buffer-substring beg end)))
     (ask-open-file-in-vscode (expand-file-name filename))))

(defun ask-open-current-project-in-vscode ()
  "Open the current project in vscode."
  (interactive)
  (let ((dir (project-root (project-current t))))
    (ask-open-file-in-vscode (expand-file-name dir))))

;; Configuration
;; Disable auto save and auto backup
(with-eval-after-load 'files
  (setq auto-save-default nil)
  (setq make-backup-files nil))

;; Recent files
(add-hook 'after-init-hook #'recentf-mode)
(with-eval-after-load 'recentf
  (setq recentf-max-saved-items 200
	recentf-exclude
        '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
          "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\)$" "^q/tmp/" "^/ssh:"
          "\\.?ido\\.last$" "\\.revive$" "/TAGS$" "^/var/folders/.+$"
          (lambda (file) (file-in-directory-p file package-user-dir))))
  (push (expand-file-name recentf-save-file) recentf-exclude))

(with-eval-after-load 'dired
  (setq dired-kill-when-opening-new-dired-buffer t)
  ;; on MacOS, ls doesn't support the --dired option while on Linux is supported.
  (when sys/macp
    (setq dired-use-ls-dired nil))
  (setq dired-guess-shell-alist-user
	`((,(rx "."
                (or
                 ;; Videos
                 "mp4" "avi" "mkv" "flv" "ogv" "mov"
                 ;; Music
                 "wav" "mp3" "flac"
                 ;; Images
                 "jpg" "jpeg" "png" "gif" "xpm" "svg" "bmp"
                 ;; Docs
                 "pdf" "md" "djvu" "ps" "eps")
                string-end)
           ,(cond ((eq system-type 'gnu/linux) "xdg-open")
                  ((eq system-type 'darwin) "open")
                  ((eq system-type 'windows-nt) "start")
                  (t "")))))
  (when (and (display-graphic-p) (not ask-start-emacs-without-packages))
    (add-hook #'dired-mode-hook 'all-the-icons-dired-mode)))

(unless ask-start-emacs-without-packages
  (with-eval-after-load 'all-the-icons
    (setq inhibit-compacting-font-caches t))
  (with-eval-after-load 'dired-sidebar
    (define-key global-map (kbd "<f8>") 'dired-sidebar-toggle-sidebar)
    ;; fix the project-root bug. more see https://github.com/jojojames/dired-sidebar/pull/72
    (defun ask-dired-sidebar-sidebar-root ()
      "Return directory using `projectile', `project' or current directory."
      (if (featurep 'projectile)
	  (condition-case nil
              (if (fboundp 'projectile-project-root)
		  (or (projectile-project-root) default-directory)
		default-directory)
            (error default-directory))
	;; Use `project' if `projectile' is not loaded yet.
	;; `projectile' is a big package and takes a while to load so it's better
	;; to defer loading it as long as possible (until the user chooses).
	(dired-sidebar-if-let* ((project (project-current)))
			       ;; https://github.com/jojojames/dired-sidebar/issues/61
			       ;; (if (eq (type-of project) 'ede-proj-project)
			       ;; Found from calling: (eieio-class-slots 'ede-proj-project)
			       ;; (slot-value project 'directory)
			       ;; e.g. (vc . "~/.emacs.d/straight/repos/dired-sidebar/")
			       ;; or (vc Git "~/.emacs.d/straight/repos/dired-sidebar/")
			       ;;(car (last project))
			       (project-root project)
			       default-directory)))
    (advice-add 'dired-sidebar-sidebar-root :override #'ask-dired-sidebar-sidebar-root)))

(provide 'ask-dired)
;;; ask-dired.el ends here
