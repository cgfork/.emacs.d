;;; package --- summary
;;; Commentary:
;;; Code:

(defun cgfork/rename-this-file-and-buffer (name)
  "Rename both current buffer and file to the NAME."
  (interactive)
  (let ((bname (buffer-name))
	(fname (buffer-file-name)))
    (unless  fname
      (error "Buffer '%s' is not a visiting file!" bname))
    (progn
      (when (file-exists-p fname)
	(rename-file fname name 1))
      (set-visited-file-name name)
      (rename-buffer name))))

(defun cgfork/delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited!"))
  (when (yes-or-no-p (format "Are you sure to delete '%s'?"
			     (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(defun cgfork/open-in-browse ()
  "Open the current file using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
	     (tramp-tramp-file-p file-name))
	(error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))

(provide '+funcs)
;;; +funcs.el ends here
