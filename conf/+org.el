;;; package --- Initialize org -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'org)

(require 'ob-plantuml)
(defun fix-plantuml-make-body (body params)
  (let ((full-body
	    (org-babel-expand-body:generic
	     body params (org-babel-variable-assignments:plantuml params))))
       (message "try to export")
       (if (string-prefix-p "@start" body t) full-body
	 (format "@startuml\n%s\n@enduml" full-body))))

(advice-add #'org-babel-plantuml-make-body :override #'fix-plantuml-make-body)

(defcustom org-notes-home (expand-file-name "~/notes")
  "Define the home path of my notes."
  :group 'cgfork
  :type 'string)

(when (eq system-type 'darwin)
  (power-emacs-install 'grab-mac-link))

;; Setup key bindings.
(define-key global-map (kbd "C-c a") 'org-agenda)
(define-key global-map (kbd "C-c c") 'org-capture)
(define-key global-map (kbd "C-c b") 'org-switchb)

(setq org-agenda-files (list org-notes-home)
      org-todo-keywords '((sequence "TODO(t)" "DOING(i)" "HANGUP(h)"
							    "|" "DONE(d)" "CANCEL(c)"))
      org-log-done 'time
      org-startup-indented nil
      org-ellipsis "  "
      org-pretty-entities t
      org-src-fontify-natively t)
  
(power-emacs-install 'ob-go :stable nil)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (go . t)
   (python . t)
   (java . t)
   (plantuml . t)
   (shell . t)))

;; Replaced by bnf-mode.
;; (define-generic-mode 'bnf-mode
;;  () ;; comment char: inapplicable because # must be at start of line
;;  nil ;; keywords
;;  '(
;;    ("^#.*" . 'font-lock-comment-face) ;; comments at start of line
;;    ("^<[^ \t\n]*?>" . 'font-lock-function-name-face) ;; LHS nonterminals
;;    ("<[^ \t\n]*?>" . 'font-lock-builtin-face) ;; other nonterminals
;;    ("::=" . 'font-lock-const-face) ;; "goes-to" symbol
;;    ("\|" . 'font-lock-warning-face) ;; "OR" symbol
;;    )
;;  '("\\.bnf\\'") ;; filename suffixes
;;  nil ;; extra function hooks
;;  "Major mode for BNF highlighting.")

(when (power-emacs-try 'bnf-mode)
  (add-to-list 'org-src-lang-modes '("bnf" . bnf)))

(when (power-emacs-try 'plantuml-mode)
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml)))

(unless (featurep 'ob) (require 'ob))
(unless (featurep 'ox-html) (require 'ox-html))
(unless (featurep 'ox-publish) (require 'ox-publish))

(defun ox-org-clean-space (text backend _)
  "Clean the space between chinese when export to html.
Replace the TEXT when the BACKEND is html."
  (when (org-export-derived-backend-p backend 'html)
    (let ((regexp "[[:multibyte:]]")
	  (s text))
      ;; 删除换行产生的空格，当使用中文的时候
      (setq s
	    (replace-regexp-in-string
	     (format "\\(%s\\) *\n *\\(%s\\)" regexp regexp)
	     "\\1\\2" s))
      ;; 删除粗体之前的空格
      (setq s
	    (replace-regexp-in-string
	     (format "\\(%s\\) +\\(<\\)" regexp)
	     "\\1\\2" s))
      (setq s
	    (replace-regexp-in-string
	     (format "\\(>\\) +\\(%s\\)" regexp)
	     "\\1\\2" s))
      s)))
(add-to-list 'org-export-filter-paragraph-functions 'ox-org-clean-space)

  ;; Rich text clipboard

(when (power-emacs-try 'org-rich-yank)
  (define-key org-mode-map (kbd "C-M-y") 'org-rich-yank))

(when (power-emacs-try 'toc-org)
  (add-hook 'org-mode-hook 'toc-org-mode))

(power-emacs-install 'org-tree-slide)
(org-tree-slide-simple-profile)
(define-key org-mode-map (kbd "C-<f7>") 'org-tree-slide-mode)
(with-eval-after-load 'org-tree-slide
  (define-key org-tree-slide-mode-map (kbd "<left>") 'org-tree-slide-move-previous-tree)
  (define-key org-tree-slide-mode-map (kbd "<right>") 'org-tree-slide-move-next-tree)
  (setq org-tree-slide-skip-outline-level 2))
  
(power-emacs-install 'htmlize)

;; Set ox-html
(setq org-html-doctype "html5"
      org-html-html5-fancy t
      org-html-metadata-timestamp-format "%Y-%m-%d %H:%M"
      org-html-inline-image-rules
      '(("file" . "\\.\\(jpeg\\|jpg\\|png\\|gif\\|svg\\)\\'")
	("http" . "\\.\\(jpeg\\|jpg\\|png\\|gif\\|svg\\)\\'")
	("https" . "\\.\\(jpeg\\|jpg\\|png\\|gif\\|svg\\)\\'")))

(let* ((notes-home org-notes-home)
       (assets (expand-file-name "assets" notes-home))
       (public (expand-file-name "public" notes-home))
       (html-public (expand-file-name "html/public" notes-home)))
  (setq org-publish-project-alist
	`(("blog-notes"
           :base-directory ,public
           :base-extension "org"
           :publishing-directory ,html-public
           :recursive t
           :publishing-function org-html-publish-to-html
           :headline-levels 4             ; Just the default for this project.
           :auto-preamble t
           :section-numbers nil
           :author "yiwen"
           :email "cg.fork@gmail.com"
           :auto-sitemap t                ; Generate sitemap.org automagically...
           :sitemap-filename "sitemap.org"  ; ... call it sitemap.org (it's the default)...
           :sitemap-title "Sitemap"         ; ... with title 'Sitemap'.
           :sitemap-sort-files anti-chronologically
           :sitemap-file-entry-format "%d %t"
           :html-head "<link rel=\"shortcut icon\" href=\"/favicon.ico\" type=\"image/x-icon\"/>
                       <link rel=\"stylesheet\" type=\"text/css\" href=\"/css/worg.css\"/>"
           :html-preamble "<div id=\"preamble\"><p class=\"preamble\">Last updated %C.</p></div>"
           :html-postamble "<div id=\"postamble\"><p class=\"postamble\">The %t published by %a with %c.</p></div>")
          ("blog-static"
           :base-directory ,public
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|ico"
           :publishing-directory ,html-public
           :recursive t
           :publishing-function org-publish-attachment)
	  ("static"
	   :base-directory ,assets
	   :base-extension "css\\|js\\|png\\|jpg\\|pdf\\|mp3\\|ogg\\|swf\\|ico"
	   :publishing-directory ,html-public
	   :recursive t
	   :publishing-function org-publish-attachment)
          ("blog" :components ("blog-notes" "blog-static" "static")))))

(let* ((org-home org-notes-home)
       (tasks-file (expand-file-name "TODOs.org" org-home))
       (journal-file (expand-file-name "Journal.org" org-home)))
  (custom-set-variables
   '(org-adapt-indentation nil)
   '(org-export-headline-levels 6)
   `(org-plantuml-jar-path ,(executable-find "plantuml.jar"))
   `(org-capture-templates
     (quote (("t" "Todo" entry (file+olp+datetree ,tasks-file)
	      "* TODO [#B] %^{Description} %^g\n%?\n%i\nAdded:%U" :time-prompt t)
	     ("T" "Todo with Clipboard" entry (file+olp+datetree ,tasks-file)
	      "* TODO [#B] %^{Description} %^g\n%c\nAdded:%U" :time-prompt t)
	     ("S" "Todo with Scheduled" entry (file+olp+datetree ,tasks-file)
	      "* TODO [#B] %^{Description} %^g\nSCHEDULED: %^t\n%?\n%i\nAdded:%U" :time-prompt t)
	     ("D" "Todo with Deadline" entry (file+olp+datetree ,tasks-file)
	      "* TODO [#B] %^{Description} %^g\nDEADLINE: %^t\n%?\n%i\nAdded:%U" :time-prompt t)
	     ("P" "TODO with Properties" entry (file+olp+datetree ,tasks-file)
	      "* TODO [#B] %^{Description} %^g\nDEADLINE: %^t\n:PROPERTIES:\n:CATEGORY: %^{Category}\n:END:\n%?\n %i\nAdded:%U" :time-prompt t)
	     ("j" "Journal" entry (file+olp ,journal-file "Journal")
	      "* %U - %^{Heading}\n %?")
	     ("l" "Log Time" entry (file+olp ,journal-file "Log Time")
	      "* %U - %^{Activity}\t :TIME:")
	     ("s" "Code Snippets" entry (file+olp ,journal-file "Code Snippets")
	      "* %U - %^{Heading} %^g\n%?\n")
	     ("c" "Contacts" table-line (file+olp ,journal-file "Contacts")
	      "| %U | %^{Name} | %^{Phone}| %^{E-mail} |"))))
   '(org-agenda-custom-commands
     '(("w" . "任务安排")
       ("wa" "重要且紧急的任务" tags-todo "+PRIORITY=\"A\"")
       ("wb" "重要且不紧急的任务" tags-todo "-Weekly-Monthly-Daily+PRIORITY=\"B\"")
       ("wc" "不重要且紧急的任务" tags-todo "+PRIORITY=\"C\"")
       ("p" . "项目安排")
       ("pw" "迭代任务" tags "CATEGORY=\"WORKLIST\"")
       ("pf" "未来要做的任务" tags-todo "CATEGORY=\"WORKLIST\"")
       ("t" . "个人任务")
       ("tw" "任务清单" tags "CATEGORY=\"TASK\"")
       ("tf" "未来要做的任务" tags-todo "CATEGORY=\"TASK\"")
       ("P" "编程" ((tags "java|go|clj|racket|js|shell|c++")
		    (tags-todo "java|go|clj|racket|js|shell|c++")))
       ("R" "提醒事项" ((tags "CATEGORY=\"REMINDER\"")
			(tags-todo "CATEGORY=\"REMINDER\"")))
       ("W" "每周工作"
	((stuck "") ;; review stuck projects as designated by org-stuck-projects
	 (tags-todo "CATEGORY=\"WORKLIST\"") ;; review all projects (assuming you use todo keywords to designate projects)
	 ))))))

(provide '+org)
;;; +org.el ends here
