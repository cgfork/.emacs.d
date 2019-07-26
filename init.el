;;; package --- summary
;;; Commentary:
;;; Code:

;; Check version
(when (version< emacs-version "25.1")
  (error "This o-init.el must require 25.1 and above!"))

;; Define consts
(defconst sys/win32p
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst sys/mac-x-p
  (and (display-graphic-p) sys/macp)
  "Are we running under X on a Mac system?")

(defconst sys/linux-x-p
  (and (display-graphic-p) sys/linuxp)
  "Are we running under X on a GNU/Linux system?")

(defconst sys/cygwinp
  (eq system-type 'cygwin)
  "Are we running on a Cygwin system?")

(defconst sys/rootp
  (string-equal "root" (getenv "USER"))
  "Are you using ROOT user?")

(defconst emacs/>=25p
  (>= emacs-major-version 25)
  "Emacs is 25 or above.")

(defconst emacs/>=26p
  (>= emacs-major-version 26)
  "Emacs is 26 or above.")

(defconst emacs/>=27p
  (>= emacs-major-version 27)
  "Emacs is 27 or above.")

(defconst emacs/>=25.2p
  (or emacs/>=26p
      (and (= emacs-major-version 25) (>= emacs-minor-version 2)))
  "Emacs is 25.2 or above.")

;; Define the group
(defgroup cgfork nil
  "Define the group for my emacs config."
  :group 'convenience)

;; Define the customize variables
(defcustom cgfork-package-archives 'netease
  "Set package archives from which to fetch."
  :type '(choice
	  (const :tag "Melpa" melpa)
	  (const :tag "Melpa Mirror" melpa-mirror)
	  (const :tag "Emacs-China" emacs-china)
	  (const :tag "Netease" netease)
	  (const :tag "Tuna" tuna))
  :group 'cgfork)

(defcustom cgfork-org-home (expand-file-name "~/.org")
  "Set the org home path."
  :type 'string
  :group 'cgfork)

;; Define variables
(defvar cgfork-gtd-file
  (concat (file-name-as-directory cgfork-org-home) (format-time-string "gtd_%Y.org")))

(defvar cgfork-record-file
  (concat (file-name-as-directory cgfork-org-home) (format-time-string "record_%Y.org")))

(defvar cgfork-snippet-file
  (concat (file-name-as-directory cgfork-org-home) (format-time-string "snippet_%Y.org")))

(defvar cgfork-journal-file
  (concat (file-name-as-directory cgfork-org-home) (format-time-string "journal_%Y.org")))

;; Define alias
;; Set list-buffers to ibuffer
(defalias 'list-buffers 'ibuffer)

;; Key Modifiers in windows
;; (when sys/win32p
;;   (setq w32-lwindow-modifier 'super)	; left win
;;   (setq w32-apps-modifier 'hyper)	; Menu key
;;   (w32-register-hot-key [s-t]))

;; Define functions
(defun cgfork-set-package-archives (archives)
  "Set specific package ARCHIVES repository."
  (interactive
   (list (intern (completing-read "Choose package archives: "
				  '(melpa melpa-mirror emacs-china netease tuna)))))
  (setq package-archives
        (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                            (not (gnutls-available-p))))
               (proto (if no-ssl "http" "https")))
          (pcase archives
            ('melpa
             `(,(cons "gnu"   (concat proto "://elpa.gnu.org/packages/"))
               ,(cons "melpa" (concat proto "://melpa.org/packages/"))))
            ('melpa-mirror
             `(,(cons "gnu"   (concat proto "://elpa.gnu.org/packages/"))
               ,(cons "melpa" (concat proto "://www.mirrorservice.org/sites/melpa.org/packages/"))))
            ('emacs-china
             `(,(cons "gnu"   (concat proto "://elpa.emacs-china.org/gnu/"))
               ,(cons "melpa" (concat proto "://elpa.emacs-china.org/melpa/"))))
            ('netease
             `(,(cons "gnu"   (concat proto "://mirrors.163.com/elpa/gnu/"))
               ,(cons "melpa" (concat proto "://mirrors.163.com/elpa/melpa/"))))
            ('tuna
             `(,(cons "gnu"   (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/"))
               ,(cons "melpa" (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))))
            (archives
             (error "Unknown archives: '%s'" archives)))))
  (message "Set package archives to '%s'." archives))

(defun cgfork-open-gtd-file()
  "Open the gtd file."
  (interactive)
  (find-file cgfork-gtd-file))

(defun cgfork-open-record-file()
  "Open the record file."
  (interactive)
  (find-file cgfork-record-file))

(defun cgfork-open-snippet-file()
  "Open the snippet file."
  (interactive)
  (find-file cgfork-snippet-file))

(defun cgfork-open-journal-file()
  "Open the journal file."
  (interactive)
  (find-file cgfork-journal-file))

;; Setup load path and use-pacakge
(add-to-list 'load-path (expand-file-name "setup" user-emacs-directory))
(cgfork-set-package-archives cgfork-package-archives)
(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


;; Setup environment for linux
(when (or sys/mac-x-p sys/linux-x-p)
  (use-package exec-path-from-shell
    :ensure t
    :init
    (setq exec-path-from-shell-check-startup-files nil)
    (setq exec-path-from-shell-variables '("PATH" "MANPATH" "PYTHONPATH" "GOPATH"))
    (setq exec-path-from-shell-arguments '("-l"))
    (exec-path-from-shell-initialize)))

;; Setup key modifier for mac
(when sys/mac-x-p
  (setq mac-command-modifier 'meta) ; make cmd key do Meta
  (setq mac-option-modifier 'super) ; make opt key do Super
  (setq mac-control-modifier 'control) ; make Control key do Control
  (setq ns-function-modifier 'hyper)  ; make Fn key do Hyper
  )

;; Setup fonts
(set-frame-font "-*-Ubuntu Mono derivative Powerline-normal-normal-normal-*-16-*-*-*-m-0-iso10646-1")

;; Setup packages
;; Start server
(use-package server
  :ensure nil
  :config
  (unless (server-running-p) (add-hook 'after-init-hook #'server-mode))
  )

;; History
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

(use-package recentf
  :ensure nil
  ;; lazy load recentf
  :hook (find-file . (lambda () (unless recentf-mode
				  (recentf-mode)
				  (recentf-track-opened-file))))
  :init
  (add-hook 'after-init-hook #'recentf-mode)
  (setq recentf-max-saved-items 200)
  :config
  (add-to-list 'recentf-exclude (expand-file-name package-user-dir))
  (add-to-list 'recentf-exclude ".cache")
  (add-to-list 'recentf-exclude ".cask")
  (add-to-list 'recentf-exclude ".elfeed")
  (add-to-list 'recentf-exclude ".idea")
  (add-to-list 'recentf-exclude ".DS_Store")
  (add-to-list 'recentf-exclude "bookmarks")
  (add-to-list 'recentf-exclude "cache")
  (add-to-list 'recentf-exclude "persp-confs")
  (add-to-list 'recentf-exclude "recentf")
  (add-to-list 'recentf-exclude "url")
  (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'"))

;; Which-key
(use-package which-key
  :ensure t
  :config (which-key-mode))

;; Try
(use-package try
  :ensure t)

;; Mutliple cursors
(use-package multiple-cursors
  :ensure t
  :bind (
	 ("M-3" . mc/mark-next-like-this)
	 ("M-4" . mc/mark-previous-like-this)
	 :map ctl-x-map
	 ("\C-m" . mc/mark-all-dwim)
	 ("<return>" . mule-keymap)
	 )
  )

;; Record the command
(use-package command-log-mode
  :ensure t
  )

;; highlight the brackets
(use-package highlight-parentheses
  :ensure t
  :init
  (highlight-parentheses-mode t))


;; Setup find-file and M-x
(use-package counsel
  :ensure t)

;; Setup navigation
(use-package avy
  :ensure t
  :config
  (avy-setup-default))

;; Setup ivy
(use-package ivy
  :ensure t
  :diminish (ivy-mode)
  :bind (("C-x b" . ivy-switch-buffer))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "%d/%d ")
  (setq ivy-display-style 'fancy))

;; Setup search
(use-package swiper
  :ensure t
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (global-set-key "\C-s" 'swiper)
    (global-set-key (kbd "C-c C-r") 'ivy-resume)
    (global-set-key (kbd "<f6>") 'ivy-resume)
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    (global-set-key (kbd "<f1> f") 'counsel-describe-function)
    (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
    (global-set-key (kbd "<f1> l") 'counsel-load-library)
    (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
    (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
    (global-set-key (kbd "C-c g") 'counsel-git)
    (global-set-key (kbd "C-c j") 'counsel-git-grep)
    (global-set-key (kbd "C-c k") 'counsel-ag)
    (global-set-key (kbd "C-x l") 'counsel-locate)
    (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
    ))

;; Setup ace-window
(use-package ace-window
  :ensure t
  :init
  (progn
    (global-set-key [remap other-window] 'ace-window)
    (custom-set-faces
     '(aw-leading-char-face
       ((t (:inherit ace-jump-face-foreground :height 3.0)))))
    ))

;; Setup dired
(use-package dired
  :ensure nil
  :config
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)

  (when sys/macp
    (setq dired-use-ls-dired nil)
    (when (executable-find "gls")
      (setq insert-directory-program "gls")))

  (when (or (and sys/macp (executable-find "gls"))
	    (and (not sys/macp) (executable-find "ls")))
    (setq dired-listing-switches "-alh -group-directories-first")
    (use-package dired-quick-sort
      :ensure nil
      :init (dired-quick-sort-setup)))

  (use-package diredfl
    :ensure nil
    :init (diredfl-global-mode 1))

  (use-package dired-aux
    :ensure nil)
  
  (use-package dired-x
    :ensure nil
    :demand
    :config
    (let ((cmd (cond
		(sys/mac-x-p "open")
		(sys/linux-x-p "xdg-open")
		(sys/win32p "start")
		(t ""))))
      (setq dired-guess-shell-alist-user
	     `(("\\.pdf\\'" ,cmd)
              ("\\.docx\\'" ,cmd)
              ("\\.\\(?:djvu\\|eps\\)\\'" ,cmd)
              ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" ,cmd)
              ("\\.\\(?:xcf\\)\\'" ,cmd)
              ("\\.csv\\'" ,cmd)
              ("\\.tex\\'" ,cmd)
              ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'"
               ,cmd)
              ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)
              ("\\.html?\\'" ,cmd)
              ("\\.md\\'" ,cmd))))

    (setq dired-omit-files
	  (concat dired-omit-files
		  "\\|^.DS_Store$\\|^.projectile$\\|^.git*\\|^.svn$\\|^.vscode$\\|\\.js\\.meta$\\|\\.meta$\\|\\.elc$\\|^.emacs.*"))
    )
  )

(use-package projectile
  :ensure t
  :config
  (projectile-mode)
  (setq projectile-completion-system 'ivy))

(use-package counsel-projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  :init
  (counsel-projectile-mode))

(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
	 ("M-g j" . dumb-jump-go)
	 ("M-g x" . dumb-jump-go-prefer-external)
	 ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'ivy) ;; (setq dumb-jump-selector 'helm)
  :init (dumb-jump-mode)
  :ensure)

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :init (yas-global-mode 1)
  :config (use-package yasnippet-snippets))

(use-package company
  :ensure t
  :diminish company-mode
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3)
  (global-company-mode t)
  )

(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "\C-n") #'company-select-next)
  (define-key company-active-map (kbd "\C-p") #'company-select-previous)
  )

(defun autoinsert-yas-expand()
  "Replace text in yasnippet template."
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))

(use-package autoinsert
  :ensure t
  :init
  ;; Don't want to be prompted before insertion:
  (setq auto-insert-query nil)
  (setq auto-insert-directory (locate-user-emacs-file "templates"))
  (add-hook 'find-file-hook 'auto-insert)
  (auto-insert-mode 1)
  :config
  (define-auto-insert "\\.org?$" [ "default-org.org" autoinsert-yas-expand ])
  (define-auto-insert "\\.el?$" [ "default-el.el" autoinsert-yas-expand ])
  (define-auto-insert "\\.sh?$" "default-sh.sh")
  )

(use-package magit
  :ensure t
  :bind (("\C-x g" . magit-status))
  )

(use-package treemacs
  :ensure t
  :defer t
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if (executable-find "python") 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-display-in-side-window        t
          treemacs-file-event-delay              5000
          treemacs-file-follow-delay             0.2
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   t
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-desc
          treemacs-space-between-root-nodes      nil
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-width                         35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    (treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag))
)

(use-package treemacs-evil
  :after treemacs evil
  :ensure t)

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(use-package plantuml-mode
  :ensure nil
  :init
  (custom-set-variables
   '(plantuml-default-exec-mode (quote jar))
   '(plantuml-jar-path (expand-file-name "~/.bin/plantuml.jar"))
   )
  :config
  (plantuml-set-exec-mode "jar"))

(use-package org
  :ensure nil
  :commands org-try-structure-completion
  :functions hydra-org-template/body
  :bind (("C-c a" . org-agenda)
         ("C-c b" . org-switchb)
	 ("C-c c" . org-capture))
  :hook (org-indent-mode . (lambda() (diminish 'org-indent-mode)))
  :config
  (setq org-agenda-files (list cgfork-org-home)
        org-todo-keywords '((sequence "TODO(t)" "DOING(i)" "HANGUP(h)"
                                      "|" "DONE(d)" "CANCEL(c)"))
        org-log-done 'time
        org-startup-indented nil
        org-ellipsis "  "
        org-pretty-entities t)

  (custom-set-variables
    '(org-plantuml-jar-path (expand-file-name "~/.bin/plantuml.jar"))
    '(org-capture-templates
      '(("t" "Todo" entry (file+datetree gtd-file)
	 "* TODO [#B] %^{Description} %^g\n%?\n%i\nAdded:%U")
	("T" "Todo with Clipboard" entry (file+datetree cgfork-gtd-file)
	 "* TODO [#B] %^{Description} %^g\n%c\nAdded:%U")
	("S" "Todo with Scheduled" entry (file+datetree cgfork-gtd-file)
	 "* TODO [#B] %^{Description} %^g\nSCHEDULED: %^t\n%?\n%i\nAdded:%U")
	("D" "Todo with Deadline" entry (file+datetree cgfork-gtd-file)
	 "* TODO [#B] %^{Description} %^g\nDEADLINE: %^t\n%?\n%i\nAdded:%U")
	("W" "Work" entry (file+datetree cgfork-gtd-file)
	 "* TODO [#B] %^{Description} :PROJECT:%^g\nDEADLINE: %^t\n:PROPERTIES:\n:CATEGORY: %^{Category}\n:END:\n%?\n %i\nAdded:%U")
	("j" "Journal" entry (file+datetree cgfork-journal-file)
	 "* %U - %^{Heading}\n %?")
	("l" "Log Time" entry (file+datetree cgfork-record-file)
	 "* %U - %^{Activity}\t :TIME:")
	("s" "Code Snippets" entry (file+datetree cgfork-snippet-file)
	 "* %U - %^{Heading}%^g\n%?\n")
	))
    '(org-agenda-custom-commands
      '(
	("w" . "任务安排")
	("wa" "重要且紧急的任务" tags-todo "+PRIORITY=\"A\"")
	("wb" "重要且不紧急的任务" tags-todo "-Weekly-Monthly-Daily+PRIORITY=\"B\"")
	("wc" "不重要且紧急的任务" tags-todo "+PRIORITY=\"C\"")
	("b" "NOTE" tags-todo "NOTE")
	("p" . "项目安排")
	("pw" "迭代任务" tags-todo "+PROJECT+CATEGORY=\"WORK\"")
	("pf" "未来要做的任务" tags-todo "+PROJECT+FUTURE+CATEGORY=\"WORK\"")
	("W" "Weekly Review"
	 ((stuck "") ;; review stuck projects as designated by org-stuck-projects
       (tags-todo "PROJECT") ;; review all projects (assuming you use todo keywords to designate projects)
       )))))
  (defadvice org-html-paragraph (before org-html-paragraph-advice
                                        (paragraph contents info) activate)
    "Join consecutive Chinese lines into a single long line without
unwanted space when exporting org-mode to html."
    (let* ((origin-contents (ad-get-arg 1))
           (fix-regexp "[[:multibyte:]]")
           (fixed-contents
            (replace-regexp-in-string
             (concat
              "\\(" fix-regexp "\\) *\n *\\(" fix-regexp "\\)") "\\1\\2" origin-contents)))
      (ad-set-arg 1 fixed-contents)))

  (add-to-list 'org-export-backends 'md)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (go . t)
     (python . t)
     (java . t)
     (plantuml . t)
     ))

  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))

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

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  )

(use-package go-mode
  :ensure t
  :bind (:map go-mode-map
	      ("\C-c R" . go-remove-unused-imports)
	      ("<f1>" . godoc-at-point)
	 )
  :commands go-mode
  :mode (("\\.go?\\'" . go-mode))
  :hook (
         (before-save . gofmt-before-save)
         )
  :config
  (setq compile-command "echo Building... && go build -v && echo Testing... && go test -v && echo Linter... && golint")
  (setq gofmt-command "goimports")
  (add-hook 'go-mode-hook (lambda ()
			    (setq-default)
			    (setq tab-width 4)
			    (setq standard-indent 2)
			    (setq indent-tabs-mode nil)
			    ))

  (use-package golint
    :ensure t
    )
  (use-package go-dlv
    :ensure t
    )
  (use-package go-rename
    :ensure t
    )
  (use-package company-go
    :ensure t
    :config
    (add-hook 'go-mode-hook (lambda ()
			      (add-to-list (make-local-variable 'company-backends)
					   '(company-go company-yasnippet company-files company-capf)
					   )))
    )
  (use-package go-eldoc
    :ensure t
    :hook (go-mode . go-eldoc-setup)
    )
  (use-package go-guru
    :ensure t
    :hook (go-mode . go-guru-hl-identifier-mode)
    )
  )

(use-package slime
  :ensure t
  :init
  :config
  (slime-setup)
  (setq inferior-lisp-program "sbcl")
  (setq slime-contribs '(slime-fancy))
  )

(use-package cider
  :ensure t
  :init)

(use-package paredit
  :ensure t
  :hook ((emacs-lisp-mode . enable-paredit-mode)
	 (lisp-mode . enable-paredit-mode)
	 (clojure-mode . enable-paredit-mode)
	 (eval-expression-minibuffer-setup . enable-paredit-mode))
  :config
  )

;; setup protobuf
(require 'protobuf-mode)
(add-hook 'protobuf-mode-hook
	  (lambda () (c-add-style "my-style" '((c-basic-offset . 2) (indent-tabs-mode . nil)) t)))

;; Set customize variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cgfork-package-archives (quote netease))
 '(column-number-mode t)
 '(current-language-environment "UTF-8")
 '(custom-enabled-themes (quote (solarized-light)))
 '(custom-safe-themes
   (quote
    ("0598c6a29e13e7112cfbc2f523e31927ab7dce56ebb2016b567e1eff6dc1fd4f" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "d91ef4e714f05fff2070da7ca452980999f5361209e679ee988e3c432df24347" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "2a7beed4f24b15f77160118320123d699282cbf196e0089f113245d4b729ba5d" "274fa62b00d732d093fc3f120aca1b31a6bb484492f31081c1814a858e25c72e" "04232a0bfc50eac64c12471607090ecac9d7fd2d79e388f8543d1c5439ed81f5" "d057f0430ba54f813a5d60c1d18f28cf97d271fd35a36be478e20924ea9451bd" default)))
 '(display-time-mode t)
 '(electric-pair-mode t)
 '(flycheck-emacs-lisp-load-path (quote inherit))
 '(global-display-line-numbers-mode t)
 '(inhibit-startup-screen t)
 '(make-backup-files nil)
 '(ns-pop-up-frames nil)
 '(org-adapt-indentation nil)
 '(org-agenda-custom-commands
   (quote
    (("w" . "任务安排")
     ("wa" "重要且紧急的任务" tags-todo "+PRIORITY=\"A\"")
     ("wb" "重要且不紧急的任务" tags-todo "-Weekly-Monthly-Daily+PRIORITY=\"B\"")
     ("wc" "不重要且紧急的任务" tags-todo "+PRIORITY=\"C\"")
     ("b" "NOTE" tags-todo "NOTE")
     ("p" . "项目安排")
     ("pw" "迭代任务" tags-todo "+PROJECT+WORK+CATEGORY=\"kaola\"")
     ("pf" "未来要做的任务" tags-todo "+PROJECT+FUTURE+CATEGORY=\"kaola\"")
     ("W" "Weekly Review"
      ((stuck "")
       (tags-todo "PROJECT"))))))
 '(org-capture-templates
   (quote
    (("t" "Todo" entry
      (file+datetree gtd-file)
      "* TODO [#B] %^{Description} %^g
%?
%i
Added:%U")
     ("T" "Todo with Clipboard" entry
      (file+datetree cgfork-gtd-file)
      "* TODO [#B] %^{Description} %^g
%c
Added:%U")
     ("S" "Todo with Scheduled" entry
      (file+datetree cgfork-gtd-file)
      "* TODO [#B] %^{Description} %^g
SCHEDULED: %^t
%?
%i
Added:%U")
     ("D" "Todo with Deadline" entry
      (file+datetree cgfork-gtd-file)
      "* TODO [#B] %^{Description} %^g
DEADLINE: %^t
%?
%i
Added:%U")
     ("W" "Work" entry
      (file+datetree cgfork-gtd-file)
      "* TODO [#B] %^{Description} :PROJECT:%^g
DEADLINE: %^t
:PROPERTIES:
:CATEGORY: %^{Category}
:END:
%?
 %i
Added:%U")
     ("j" "Journal" entry
      (file+datetree cgfork-journal-file)
      "* %U - %^{Heading}
 %?")
     ("l" "Log Time" entry
      (file+datetree cgfork-record-file)
      "* %U - %^{Activity}	 :TIME:")
     ("s" "Code Snippets" entry
      (file+datetree cgfork-snippet-file)
      "* %U - %^{Heading}%^g
%?
"))))
 '(org-export-headline-levels 6)
 '(org-plantuml-jar-path (expand-file-name "~/.bin/plantuml.jar"))
 '(package-selected-packages
   (quote
    (ac-html doom-modeline spacemacs-theme solarized-theme lsp-ui lsp-mode ample-zen-theme dracula-theme dired-sidebar cider paredit go-rename go-dlv golint highlight-parentheses slime smex neotree command-log-mode zenburn-theme treemacs-magit treemacs-icons-dired treemacs-projectile treemacs-evil treemacs go-guru go-eldoc company-go multiple-cursors ob-go org-preview-html go-mode counsel-projectile projectile diredfl all-the-icons-dired pcre2el dired+ yasnippet-snippets company counsel ace-window exec-path-from-shell try use-package)))
 '(plantuml-default-exec-mode (quote jar))
 '(plantuml-jar-path (expand-file-name "~/.bin/plantuml.jar"))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tab-stop-list
   (quote
    (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120)))
 '(tool-bar-mode nil)
 '(winner-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))

(provide 'init)
;;; init.el ends here
