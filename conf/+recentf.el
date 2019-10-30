;;; package --- summary
;;; Commentary:
;;; Code:

;; Set recentf
(with-eval-after-load 'recentf
  (add-hook 'after-init-hook #'recentf-mode)
  (setq recentf-max-saved-items 200
	recentf-exclude
        '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
          "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\)$" "^/tmp/" "^/ssh:"
          "\\.?ido\\.last$" "\\.revive$" "/TAGS$" "^/var/folders/.+$"
          (lambda (file) (file-in-directory-p file package-user-dir))))
  (push (expand-file-name recentf-save-file) recentf-exclude))

(with-eval-after-load 'savehist
  (add-hook 'after-init-hook #'savehist-mode)
   (setq enable-recursive-minibuffers t 
         history-length 1000
         savehist-additional-variables '(mark-ring
                                        global-mark-ring
                                        search-ring
                                        regexp-search-ring
                                        extended-command-history)
         savehist-autosave-interval 300))
(provide '+recentf)
;;; +recentf.el ends here
