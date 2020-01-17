;; package --- summary
;;; Commentary:
;;; Code:

(when (version< emacs-version "25.1")
  (error "This requires Emacs 25.1 or above!"))

(defun cgfork/add-load-path (&rest _)
  "Add 'conf' and 'site-conf' to `load-path'."
  (add-to-list 'load-path (expand-file-name "site-conf" user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "conf" user-emacs-directory)))

(defun cgfork/add-subdirs-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (let ((default-directory
	  (expand-file-name "site-conf" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'cgfork/add-load-path)
(advice-add #'package-initialize :after #'cgfork/add-subdirs-load-path)

(cgfork/add-load-path)

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
(defcustom cgfork/package-archives 'tuna
  "Set package archives from which to fetch."
  :type '(choice
	  (const :tag "Melpa" melpa)
	  (const :tag "Melpa Mirror" melpa-mirror)
	  (const :tag "Emacs-China" emacs-china)
	  (const :tag "Netease" netease)
	  (const :tag "Tuna" tuna))
  :group 'cgfork)

(defcustom cgfork/org-home (expand-file-name "~/Prophet")
  "Set the org home path."
  :type 'string
  :group 'cgfork)

(defcustom cgfork/tasks-file (expand-file-name "getting-things-done.org" cgfork/org-home)
  "Set the gtd file."
  :type 'string
  :group 'cgfork)

(defcustom cgfork/journal-file (expand-file-name "journal.org" cgfork/org-home)
  "Set the journal file."
  :type 'string
  :group 'cgfork)

;; Load `custom-file'.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; If it doesn't exist, copy from the template, then load it.
(let ((custom-template-file (expand-file-name "custom-template.el" user-emacs-directory)))
  (if (and (file-exists-p custom-template-file)
	   (not (file-exists-p custom-file)))
      (copy-file custom-template-file custom-file)))
(if (file-exists-p custom-file)
    (load custom-file)
  (message "custom file is not exist!"))

;; Load `custom-post.el'.
(add-hook 'after-init-hook
	  (lambda ()
	    (let ((file (expand-file-name "custom-post.el" user-emacs-directory)))
	      (when (file-exists-p file)
		  (load file)))))

;; Set Key Modifiers.
(with-no-warnings
  (cond
   (sys/win32p
    (setq w32-lwindow-modifier 'super ; Left windows key
	  w32-apps-modifier 'hyper) ; Menu key
    (w32-register-hot-key [s-t]))
   (sys/mac-x-p
    (setq mac-option-modifier 'super ; option
	  mac-command-modifier 'meta ; command
	  mac-control-modifier 'control ; control
	  ns-function-modifier 'hyper)))) ; fn

;; HACK: DO NOT copy package-selected-packages to init/custom file forcibly.
;; https://github.com/jwiegley/use-package/issues/383#issuecomment-247801751
(defun cgfork/save-selected-packages (&optional value)
  "Set `package-selected-packages' to VALUE but don't save to `custom-file'."
  (when value
    (setq package-selected-packages value)))
;; (advice-add 'package--save-selected-packages :override #'cgfork/save-selected-packages)

(defun cgfork/set-package-archives (archives)
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

;; Set package archives.
(cgfork/set-package-archives cgfork/package-archives)

;; Set list-buffers to ibuffer
(defalias 'list-buffers 'ibuffer)

(require 'package)
(require 'cl-lib)

(defun cgfork/install (package &optional min-version no-refresh)
  "Install the PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available pacakge lists will
not be re-downloaded in order to locate PACKAGE."
  (or (package-installed-p package min-version)
      (let* ((known (cdr (assoc package package-archive-contents)))
	     (versions (mapcar #'package-desc-version known)))
	(if (cl-find-if (lambda (v) (version-list-<= min-version v)) versions)
	    (package-install package)
	  (if no-refresh
	      (error "No version of %s >= %S is available" package min-version)
	    (package-refresh-contents)
	    (cgfork/install package min-version t))))))

(defun cgfork/try-install (package &optional min-version no-refresh)
  "Try to install PACKAGE, and return non-nil if successful.
If it is failure, return nil and display a warning message.
Optionally require MIN-VERSION.  If NO-REFRESH is non-nil, the
available package lists will not be re-downloaded in order to
locate PACKAGE."
  (interactive
   (progn
     ;; Initialize the package system to get the list of package
     ;; symbols for completion.
     (unless package--initialized
       (package-initialize t))
     (unless package-archive-contents
       (package-refresh-contents))
     (list (intern (completing-read
                    "Install package: "
                    (delq nil
                          (mapcar (lambda (elt)
                                    (unless (package-installed-p (car elt))
                                      (symbol-name (car elt))))
                                  package-archive-contents))
                    nil t))
           nil)))
  (condition-case err
      (cgfork/install package min-version no-refresh)
    (error
     (message "Couldn't install optional package `%s': %S" package err)
     nil)))

(defmacro cgfork/after-load (file &rest body)
  "Print the message after loading the FILE, and then eval the BODY."
  (declare (indent 1) (debug t))
  `(with-eval-after-load ,file (message "Emacs Init: '%s' is loaded." ,file) ,@body))

;; package.el updates the saved version of package-selected-packages correctly only
;; after custom-file has been loaded, which is a bug. We work around this by adding
;; the required packages to package-selected-packages after startup is complete.
(defvar cgfork/installed-packages nil)

(defun cgfork/note-selected-package (oldfun package &rest args)
  "If OLDFUN reports PACKAGE with ARGS was successfully installed, note it in `cgfork/installed-packages'."
  (let ((available (apply oldfun package args)))
    (prog1 available ;; return available
      (when (and available (boundp 'package-selected-packages))
        (add-to-list 'cgfork/installed-packages package)))))

(advice-add 'cgfork/install :around 'cgfork/note-selected-package)

;; Initialize packages.
(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil)
  (package-initialize)
  (message "Package Initialized!"))

(when (fboundp 'package--save-selected-packages)
  (cgfork/install 'seq)
  (add-hook 'after-init-hook
	    (lambda () (package--save-selected-packages
			(seq-uniq (append cgfork/installed-packages package-selected-packages))))))

;;;;;;;;;;;;;;;;;;;;;; Basic Packages ;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set `diminish'.
(cgfork/install 'diminish)

;; Keep keyring updated.
(cgfork/install 'gnu-elpa-keyring-update)

;; Auto update packages.
(when (cgfork/try-install 'auto-package-update)
  (defalias 'upgrade-packages #'auto-package-update-now)
  (cgfork/after-load 'auto-package-update
    (setq auto-package-update-delete-old-versions t
          auto-package-update-hide-results t)))

;; Setup environment for linux.
(when (or sys/mac-x-p sys/linux-x-p)
  (cgfork/install 'exec-path-from-shell)
  (cgfork/after-load 'exec-path-from-shell
    (setq exec-path-from-shell-check-startup-files nil)
    (setq exec-path-from-shell-variables '("PATH" "MANPATH" "PYTHONPATH" "GOPATH" "JAVA_HOME"))
    (setq exec-path-from-shell-arguments '("-l")))
  (exec-path-from-shell-initialize))

;; Setup paredit for lisp programming.
;; If you want to open paredit mode, you should add the hook
;; to `enable-paredit-mode'.
(cgfork/install 'paredit)
(with-eval-after-load 'paredit
  (add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode))

;; A macro for openning paredit.
(defmacro cgfork/open-paredit (mode-hook)
  "Add the `enable-paredit-mode' to the specific MODE-HOOK."
  `(add-hook ,mode-hook 'enable-paredit-mode))

;; Highlight parentheses.
(when (cgfork/try-install 'highlight-parentheses)
  (global-highlight-parentheses-mode 1))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require '+gui)
(require '+funcs)
(require '+server)
(require '+recentf)
(require '+try)
(require '+which-key)
(require '+dired)
(require '+multiple-cursors)
(require '+avy)
(require '+all-the-icons)
(require '+yasnippet)
(require '+ivy)
(require '+emacs-lisp)
(require '+clojure)
(require '+projectile)
(require '+ibuffer)
(require '+company)
(require '+grep)
(require '+magit)
(require '+plantuml)
(require '+org)
(require '+markdown)
(require '+flycheck)
(require '+lsp)
(require '+go)
(require '+tree)
(require '+yaml)
(require '+racket)

;; setup protobuf
(require 'protobuf-mode)
(add-to-list 'auto-mode-alist (cons "\\.proto\\'" 'protobuf-mode))
(add-hook 'protobuf-mode-hook
	  (lambda () (c-add-style "my-style" '((c-basic-offset . 2) (indent-tabs-mode . nil)) t)))

;; (provide 'init)

;;; Init.el ends here
