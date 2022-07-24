;;; ask-headerline.el --- Header Line -*- lexical-binding: t -*-

;; Copyright (C) 2021 cgfork

;; Author: cgfork(cg.fork@gmail.com)
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

;; Define

(defun ask-headline-format ()
  "Return the header line format."
  (let* ((fullname (abbreviate-file-name buffer-file-name))
	 (fulldir (file-name-directory fullname))
	 (dropstr "[...]"))
    (if (> (length fullname) (window-body-width))
	(if (> (length fulldir) (window-body-width))
	    (progn
	      (concat (propertize dropstr 'face '(:background "blue"))
		      (propertize (substring fulldir
					     (+ (- (length fulldir) (window-body-width))
						(length dropstr))
					     (length fulldir))
				  'face 'bold)))
	  (concat (propertize fulldir 'face '(:foreground "#8fb28f"))))
      (concat (propertize fulldir 'face '(:foreground "#8fb28f")) ;; wrong '(:foreground "#8fb28f" :weight 'bold), why?
	      (propertize (file-name-nondirectory fullname)
			  'face 'bold)))))

;; Configuration
(add-hook 'buffer-list-update-hook (lambda ()
				     (setq header-line-format
					   `(""
					     (:evel ,(if (buffer-file-name)
							(ask-headline-format)
						      "%b"))))))

(provide 'ask-headerline)
;;; ask-headerline.el ends here
