;;; package --- summary
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'setup-const))

;; my workflow
(setq org-home "~/.org")

(setq gtd-file (concat (file-name-as-directory org-home) (format-time-string "gtd_%Y.org")))
(setq record-file (concat (file-name-as-directory org-home) (format-time-string "record_%Y.org")))
(setq snippet-file (concat (file-name-as-directory org-home) (format-time-string "snippet_%Y.org")))
(setq journal-file (concat (file-name-as-directory org-home) (format-time-string "journal_%Y.org")))

(defun open-gtd-file()
  "Open the gtd file."
  (interactive)
  (find-file gtd-file))

(defun open-record-file()
  "Open the record file."
  (interactive)
  (find-file record-file))

(defun open-snippet-file()
  "Open the snippet file."
  (interactive)
  (find-file snippet-file))

(defun open-journal-file()
  "Open the journal file."
  (interactive)
  (find-file journal-file))

(use-package org
  :ensure nil
  :commands org-try-structure-completion
  :functions hydra-org-template/body
  :bind (("C-c a" . org-agenda)
         ("C-c b" . org-switchb)
	 ("C-c c" . org-capture))
  :hook (org-indent-mode . (lambda() (diminish 'org-indent-mode)))
  :config
  (setq org-agenda-files (list org-home)
        org-todo-keywords '((sequence "TODO(t)" "DOING(i)" "HANGUP(h)"
                                      "|" "DONE(d)" "CANCEL(c)"))
        org-log-done 'time
        org-startup-indented nil
        org-ellipsis "  "
        org-pretty-entities t
        org-hide-emphasis-markers t)
  (setq org-capture-templates
	'(("t" "Todo" entry (file+datetree gtd-file)
           "* TODO [#B] %^{Description} %^g\n%?\n%i\nAdded:%U")
	  ("T" "Todo with Clipboard" entry (file+datetree gtd-file)
           "* TODO [#B] %^{Description} %^g\n%c\nAdded:%U")
	  ("S" "Todo with Scheduled" entry (file+datetree gtd-file)
           "* TODO [#B] %^{Description} %^g\nSCHEDULED: %^t\n%?\n%i\nAdded:%U")
	  ("D" "Todo with Deadline" entry (file+datetree gtd-file)
           "* TODO [#B] %^{Description} %^g\nDEADLINE: %^t\n%?\n%i\nAdded:%U")
	  ("W" "Work" entry (file+datetree gtd-file)
           "* TODO [#B] %^{Description} :PROJECT:%^g\nDEADLINE: %^t\n:PROPERTIES:\n:CATEGORY: %^{Category}\n:END:\n%?\n %i\nAdded:%U")
          ("j" "Journal" entry (file+datetree journal-file)
           "* %U - %^{Heading}\n %?")
	  ("l" "Log Time" entry (file+datetree record-file)
	   "* %U - %^{Activity}\t :TIME:")
	  ("s" "Code Snippets" entry (file+datetree snippet-file)
	   "* %U - %^{Heading}%^g\n%?\n")
	  )
	)
  (setq org-agenda-custom-commands
        '(
          ("w" . "任务安排")
          ("wa" "重要且紧急的任务" tags-todo "+PRIORITY=\"A\"")
          ("wb" "重要且不紧急的任务" tags-todo "-Weekly-Monthly-Daily+PRIORITY=\"B\"")
          ("wc" "不重要且紧急的任务" tags-todo "+PRIORITY=\"C\"")
          ("b" "NOTE" tags-todo "NOTE")
          ("p" . "项目安排")
          ("pw" "迭代任务" tags-todo "+PROJECT+WORK+CATEGORY=\"kaola\"")
          ("pf" "未来要做的任务" tags-todo "+PROJECT+FUTURE+CATEGORY=\"kaola\"")
          ("W" "Weekly Review"
           ((stuck "") ;; review stuck projects as designated by org-stuck-projects
            (tags-todo "PROJECT") ;; review all projects (assuming you use todo keywords to designate projects)
            ))))
  
  (add-to-list 'org-export-backends 'md)

  ;; org-babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (go . t)
     (python . t)
     (java . t)
   ))

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
  (add-to-list 'org-src-lang-modes '("data-model" . data-model))
  (define-generic-mode
      'data-model-mode ;; mode name
    '("%") ;; comments start '%
    '("string" "int" "float" "int64" "int32" "bool" "map") ;; keywords
    '(("@" . 'font-lock-builtin) ;; is builtin
      ("!" . 'font-lock-operator) ;; is operator
      ("%{\\([A-Z_]+\\)}" . font-lock-variable-name-face)
      )
    '("\\.dm$") ;; file to activate this mode
    nil ;; functions to call
    "A mode for defining a data model"
    )
  (add-hook 'data-model-mode
	    (function
	     (lambda ()
	       (setq indent-tabs-mode nil)
	       (setq tab-width 4)
	       ))
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
