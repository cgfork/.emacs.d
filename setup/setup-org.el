;;; package --- summary
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'setup-const))

;; my workflow
(setq cgfork-job-home "~/.job")

(setq job-plan-file (concat (file-name-as-directory cgfork-job-home) (format-time-string "job_plan_%Y.org")))
(setq job-time-file (concat (file-name-as-directory cgfork-job-home) (format-time-string "job_time_%Y.org")))
(setq job-snippet-file (concat (file-name-as-directory cgfork-job-home) (format-time-string "job_snippet_%Y.org")))
(setq job-journal-file (concat (file-name-as-directory cgfork-job-home) (format-time-string "job_journal_%Y.org")))

(defun open-job-plan-file()
  (interactive)
  (find-file job-plan-file))

(defun open-job-time-file()
  (interactive)
  (find-file job-time-file))

(defun open-job-snippet-file()
  (interactive)
  (find-file job-snippet-file))

(defun open-job-journal-file()
  (interactive)
  (find-file job-journal-file))

(use-package org
  :ensure nil
  :commands org-try-structure-completion
  :functions hydra-org-template/body
  :bind (("C-c a" . org-agenda)
         ("C-c b" . org-switchb)
	 ("C-c c" . org-capture))
  :hook (org-indent-mode . (lambda() (diminish 'org-indent-mode)))
  :config
  (setq org-agenda-files '("~/note/agenda")
        org-todo-keywords '((sequence "TODO(t)" "DOING(i)" "HANGUP(h)"
                                      "|" "DONE(d)" "CANCEL(c)"))
        org-log-done 'time
        org-startup-indented nil
        org-ellipsis "  "
        org-pretty-entities t
        org-hide-emphasis-markers t)
  (setq org-capture-templates
	'(("t" "Todo" entry (file+datetree job-plan-file)
           "* TODO %^{Description} %^g\n %?\n %i\n Added:%U")
	  ("T" "Todo with Clipboard" entry (file+datetree job-plan-file)
           "* TODO %^{Description} %^g\n %c\n Added:%U")
	  ("s" "Todo with Scheduled" entry (file+datetree job-plan-file)
           "* TODO %^{Description} %^g\n SCHEDULED: %^t\n %?\n %i\n Added:%U")
	  ("d" "Todo with Deadline" entry (file+datetree job-plan-file)
           "* TODO %^{Description} %^g\n DEADLINE: %^t\n %?\n %i\n Added:%U")
          ("j" "Journal" entry (file+datetree job-journal-file)
           "* %U - %^{Heading}\n %?")
	  ("l" "Log Time" entry (file+datetree job-time-file)
	   "* %U - %^{Activity}\t :TIME:")
	  ("s" "Code Snippets" entry (file+datetree job-snippet-file)
	   "* %U - %^{Heading} %^g\n %?\n")
	  )
	)
  
  (add-to-list 'org-export-backends 'md)

  ;; Rich text clipboard
  (use-package org-rich-yank
    :bind (:map org-mode-map
                ("C-M-y" . org-rich-yank)))

  ;; Preview
  (use-package org-preview-html
    :diminish org-preview-html-mode)

  (use-package ob
    :config
    (setq org-src-fontify-natively t)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((js . t)
       (go . t)
       (org . t)
       (java . t)
       (shell . t)
       (python . t)
       (emacs-lisp .t)))
    )

  (use-package ox-md
    :config
    (setq org-md-headline-style 'atx)
    (use-package ox-gfm
      :ensure t)
    )

  (use-package ox-html
    :config
    (setq org-html-doctype "html5"
	  org-html-html5-fancy t
	  org-html-metadata-timestamp-format "%Y-%m-%d %H:%M")
    (setq org-html-inline-image-rules
          '(
            ("file" . "\\.\\(jpeg\\|jpg\\|png\\|gif\\|svg\\)\\'")
            ("http" . "\\.\\(jpeg\\|jpg\\|png\\|gif\\|svg\\)\\'")
            ("https" . "\\.\\(jpeg\\|jpg\\|png\\|gif\\|svg\\)\\'")
	    )
	  )
    )
  )

(use-package ox-publish
  :config
  (use-package htmlize
    :ensure t
  )

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
          ("draft" :components ("draft-notes" "draft-static"))
          ))
  )


(provide 'setup-org)
;;; setup-org.el ends here
