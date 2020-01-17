;;; package --- Initialize Recentf -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Set recentf
(add-hook 'after-init-hook #'recentf-mode)
(cgfork/after-load 'recentf
  (setq recentf-max-saved-items 200
	recentf-exclude
        '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
          "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\)$" "^/tmp/" "^/ssh:"
          "\\.?ido\\.last$" "\\.revive$" "/TAGS$" "^/var/folders/.+$"
          (lambda (file) (file-in-directory-p file package-user-dir))))
  (push (expand-file-name recentf-save-file) recentf-exclude))

(add-hook 'after-init-hook #'savehist-mode)
(cgfork/after-load 'savehist
   (setq enable-recursive-minibuffers t 
         history-length 1000
         savehist-additional-variables '(mark-ring
                                        global-mark-ring
                                        search-ring
                                        regexp-search-ring
                                        extended-command-history)
         savehist-autosave-interval 300))

(defun cgfork/open-recentf ()
  "Open the recentf FIlE."
  (interactive)
  (if (not (featurep 'recentf))
    (message "recentf-mode is disabled")
    (ivy-read "Recentf: " recentf-list
	      :require-match t
	      :action (lambda (filename)
			(find-file filename))
	      :caller 'cgfork/open-recentf)))

(define-key global-map (kbd "C-x C-r") 'cgfork/open-recentf)

(provide '+recentf)
;;; +recentf.el ends here
