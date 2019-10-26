;;; package --- summary
;;; Commentary:
;;; Code:

(use-package org
  :defines cgfork-org-home
  :bind (("C-c a" . org-agenda)
	 ("C-c b" . org-switchb)
	 ("C-c c" . org-capture))
  :config
  (setq org-agenda-files (list cgfork-org-home)
        org-todo-keywords '((sequence "TODO(t)" "DOING(i)" "HANGUP(h)"
                                      "|" "DONE(d)" "CANCEL(c)"))
        org-log-done 'time
        org-startup-indented nil
        org-ellipsis "  "
        org-pretty-entities t
	org-src-fontify-natively t)
  
  (defadvice org-html-paragraph (before org-html-paragraph-advice
					(paragraph contents info) activate)
    "Join consecutive Chinese lines into a single long line without unwanted space when exporting org-mode to html."
    (let* ((origin-contents (ad-get-arg 1))
           (fix-regexp "[[:multibyte:]]")
           (fixed-contents
            (replace-regexp-in-string
             (concat
              "\\(" fix-regexp "\\) *\n *\\(" fix-regexp "\\)") "\\1\\2" origin-contents)))
      (ad-set-arg 1 fixed-contents)))
  
  (add-to-list 'org-export-backends 'md)
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (go . t)
     (python . t)
     (java . t)
     (plantuml . t)
     (shell . t)))
  ;; Rich text clipboard
  (use-package org-rich-yank
    :ensure t
    :bind (:map org-mode-map
                ("C-M-y" . org-rich-yank)))
  ;; Preview
  (use-package org-preview-html
    :ensure t
    :diminish org-preview-html-mode)

   ;; Table of contents
  (use-package toc-org
    :ensure t
    :hook (org-mode . toc-org-mode))

    ;; Presentation
  (use-package org-tree-slide
    :diminish
    :functions (org-display-inline-images
                org-remove-inline-images)
    :bind (:map org-mode-map
           ("C-<f7>" . org-tree-slide-mode)
           :map org-tree-slide-mode-map
           ("<left>" . org-tree-slide-move-previous-tree)
           ("<right>" . org-tree-slide-move-next-tree)
           ("S-SPC" . org-tree-slide-move-previous-tree)
           ("SPC" . org-tree-slide-move-next-tree))
    :hook ((org-tree-slide-play . (lambda ()
                                    (text-scale-increase 4)
                                    (org-display-inline-images)
                                    (read-only-mode 1)))
           (org-tree-slide-stop . (lambda ()
                                    (text-scale-increase 0)
                                    (org-remove-inline-images)
                                    (read-only-mode -1))))
    :config
    (org-tree-slide-simple-profile)
    (setq org-tree-slide-skip-outline-level 2))

  (use-package ob-go
    :ensure t))

(use-package ox-publish
  :config
  (use-package htmlize
    :ensure t)
  
  (use-package ox-html
    :config
    (setq org-html-doctype "html5"
	  org-html-html5-fancy t
	  org-html-metadata-timestamp-format "%Y-%m-%d %H:%M")
    (setq org-html-inline-image-rules
          '(("file" . "\\.\\(jpeg\\|jpg\\|png\\|gif\\|svg\\)\\'")
            ("http" . "\\.\\(jpeg\\|jpg\\|png\\|gif\\|svg\\)\\'")
            ("https" . "\\.\\(jpeg\\|jpg\\|png\\|gif\\|svg\\)\\'"))))
  
  (setq org-publish-project-alist
	'(
          ("blog-notes"
           :base-directory "~/note/notes"
           :base-extension "org"
           :publishing-directory "~/note/public_html/"
           :recursive t
           :publishing-function org-html-publish-to-html
           :headline-levels 4             ; Just the default for this project.
           :auto-preamble t
           :section-numbers nil
           :author "C_G"
           :email "cg.fork@gmail.com"
           :auto-sitemap t                ; Generate sitemap.org automagically...
           :sitemap-filename "sitemap.org"  ; ... call it sitemap.org (it's the default)...
           :sitemap-title "Sitemap"         ; ... with title 'Sitemap'.
           :sitemap-sort-files anti-chronologically
           :sitemap-file-entry-format "%d %t"
           :html-head "<link rel=\"shortcut icon\" href=\"/favicon.ico\" type=\"image/x-icon\"/>
                       <link rel=\"stylesheet\" type=\"text/css\" href=\"/css/worg.css\"/>"
           :html-preamble "<div id=\"preamble\"><p class=\"preamble\">Last updated %C.</p></div>"
           :html-postamble "<div id=\"postamble\"><p class=\"postamble\">The %t published by %a with %c.</p></div>"
           )
          ("blog-static"
           :base-directory "~/note/notes"
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|ico"
           :publishing-directory "~/note/public_html/"
           :recursive t
           :publishing-function org-publish-attachment
           )
          ("blog" :components ("blog-notes" "blog-static"))
          ("draft-notes"
           :base-directory "~/note/draft"
           :base-extension "org"
           :publishing-directory "~/note/draft_html/"
           :recursive t
           :publishing-function org-html-publish-to-html
           :headline-levels 4             ; Just the default for this project.
           :auto-preamble t
           :section-numbers nil
           :author "C_G"
           :email "cg.fork@gmail.com"
           :auto-sitemap t                ; Generate sitemap.org automagically...
           :sitemap-filename "sitemap_draft.org"  ; ... call it sitemap.org (it's the default)...
           :sitemap-title "Sitemap"         ; ... with title 'Sitemap'.
           :sitemap-sort-files anti-chronologically
           :sitemap-file-entry-format "%d %t"
           :html-head "<link rel=\"shortcut icon\" href=\"/favicon.ico\" type=\"image/x-icon\"/>
                       <link rel=\"stylesheet\" type=\"text/css\" href=\"/css/worg.css\"/>"
           :html-preamble "<div id=\"preamble\"><p class=\"preamble\">Last updated %C.</p></div>"
           :html-postamble "<div id=\"postamble\"><p class=\"postamble\">The %t published by %a with %c.</p></div>"
           )
          ("draft-static"
           :base-directory "~/note/draft"
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|ico"
           :publishing-directory "~/note/draft_html/"
           :recursive t
           :publishing-function org-publish-attachment
           )
          ("draft" :components ("draft-notes" "draft-static")))))

(provide '+org)
;;; +org.el ends here
