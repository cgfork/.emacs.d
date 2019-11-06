;;; package --- All The Icons -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Icons
;; NOTE: Must run `M-x all-the-icons-install-fonts', and install fonts manually on Windows
(when (display-graphic-p)
  (when (cgfork/try-install 'all-the-icons)
    (unless (or sys/win32p (member "all-the-icons" (font-family-list)))
      (all-the-icons-install-fonts t))
    (with-eval-after-load 'all-the-icons
      (add-to-list 'all-the-icons-mode-icon-alist
		   '(vterm-mode all-the-icons-octicon "terminal" :v-adjust 0.2))
      (add-to-list 'all-the-icons-icon-alist
		   '("\\.xpm$" all-the-icons-octicon "file-media" :v-adjust 0.0 :face all-the-icons-dgreen))
      (add-to-list 'all-the-icons-icon-alist
		   '("\\.toml$" all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-dyellow))
      (add-to-list 'all-the-icons-mode-icon-alist
		   '(conf-toml-mode all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-dyellow))
      (add-to-list 'all-the-icons-icon-alist
		   '("\\.lua$" all-the-icons-fileicon "lua" :face all-the-icons-dblue))
      (add-to-list 'all-the-icons-mode-icon-alist
		   '(lua-mode all-the-icons-fileicon "lua" :face all-the-icons-dblue))
      (add-to-list 'all-the-icons-mode-icon-alist
		   '(help-mode all-the-icons-faicon "info-circle" :height 1.1 :v-adjust -0.1 :face all-the-icons-purple))
      (add-to-list 'all-the-icons-mode-icon-alist
		   '(helpful-mode all-the-icons-faicon "info-circle" :height 1.1 :v-adjust -0.1 :face all-the-icons-purple))
      (add-to-list 'all-the-icons-mode-icon-alist
		   '(Info-mode all-the-icons-faicon "info-circle" :height 1.1 :v-adjust -0.1))
      (add-to-list 'all-the-icons-icon-alist
		   '("NEWS$" all-the-icons-faicon "newspaper-o" :height 0.9 :v-adjust -0.2))
      (add-to-list 'all-the-icons-icon-alist
		   '("Cask\\'" all-the-icons-fileicon "elisp" :height 1.0 :v-adjust -0.2 :face all-the-icons-blue))
      (add-to-list 'all-the-icons-mode-icon-alist
		   '(cask-mode all-the-icons-fileicon "elisp" :height 1.0 :v-adjust -0.2 :face all-the-icons-blue))
      (add-to-list 'all-the-icons-icon-alist
		   '(".*\\.ipynb\\'" all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-orange))
      (add-to-list 'all-the-icons-mode-icon-alist
		   '(ein:notebooklist-mode all-the-icons-faicon "book" :face all-the-icons-orange))
      (add-to-list 'all-the-icons-mode-icon-alist
		   '(ein:notebook-mode all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-orange))
      (add-to-list 'all-the-icons-mode-icon-alist
		   '(ein:notebook-multilang-mode all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-orange))
      (add-to-list 'all-the-icons-icon-alist
		   '("\\.epub\\'" all-the-icons-faicon "book" :height 1.0 :v-adjust -0.1 :face all-the-icons-green))
      (add-to-list 'all-the-icons-mode-icon-alist
		   '(nov-mode all-the-icons-faicon "book" :height 1.0 :v-adjust -0.1 :face all-the-icons-green))
      (add-to-list 'all-the-icons-mode-icon-alist
		   '(gfm-mode all-the-icons-octicon "markdown" :face all-the-icons-lblue)))))

(provide '+all-the-icons)
;;; +all-the-icons.el ends here
