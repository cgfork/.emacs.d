;;; ask-emacs.el --- Emacs basic configuration -*- lexical-binding: t -*-

;; Copyright (C) 2021 cgfork

;; Author: cgfork(cg.fork@gmail.com)
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'server)

(with-eval-after-load 'server
  (unless (server-running-p)
    (add-hook 'after-init-hook #'server-mode)))

;; Enable the mouse support if the emacs is running in the Terminal.
(unless (display-graphic-p)
  (require 'mouse)
  (xterm-mouse-mode t))

;; flullscreen
(when (display-graphic-p)
  (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

(add-hook 'after-init-hook (lambda ()
			     (line-number-mode)
			     (column-number-mode)
			     (size-indication-mode)
			     (visual-line-mode)
			     (global-hl-line-mode)
			     (global-so-long-mode)
			     (global-auto-revert-mode)
			     (save-place-mode)
			     (delete-selection-mode)
			     (show-paren-mode)
			     (pixel-scroll-precision-mode)
			     (global-subword-mode) ;; move cursor into between camelCaseWords
			     ;; (global-superword-mode) ;; change all cursor movement/edit commands to treat text as snake_case.
			     ;; (global-prettify-symbols-mode 1) ;; show lambda as λ => set prettify-symbols-alist to change the mapping.
			     ))

(global-set-key (kbd "C-c C-b") 'ibuffer)

;; History
(add-hook 'after-init-hook #'savehist-mode)
(with-eval-after-load 'savehist
  (setq enable-recursive-minibuffers t
	history-length 1000
	savehist-additional-variables '(mark-ring
					global-mark-ring
					search-ring
					regexp-search-ring
					extended-command-history)
	savehist-autosave-interval 300))

;; Web jump
(with-eval-after-load 'webjump
  (setq webjump-sites '(
			;; Emacs.
			("Emacs Home Page" .
			 "www.gnu.org/software/emacs/emacs.html")
			("Savannah Emacs page" .
			 "savannah.gnu.org/projects/emacs")
			;; Internet search engines.
			("DuckDuckGo" .
			 [simple-query "duckduckgo.com"
                                       "duckduckgo.com/?q=" ""])
			("Google" .
			 [simple-query "www.google.com"
                                       "www.google.com/search?q=" ""])
			("Google Groups" .
			 [simple-query "groups.google.com"
                                       "groups.google.com/groups?q=" ""])
			("Wikipedia" .
			 [simple-query "wikipedia.org" "wikipedia.org/wiki/" ""]))))

;; Calendar
(with-eval-after-load 'calendar
  ;; 分别是妇女节、植树节、劳动节、青年节、儿童节、教师节、国庆节、程序员节、双11
  (setq holiday-local-holidays `((holiday-fixed 3 8  "Women's Day")
				 (holiday-fixed 3 12 "Arbor Day")
				 ,@(cl-loop for i from 1 to 3
                                            collect `(holiday-fixed 5 ,i "International Workers' Day"))
				 (holiday-fixed 5 4  "Chinese Youth Day")
				 (holiday-fixed 6 1  "Children's Day")
				 (holiday-fixed 9 10 "Teachers' Day")
				 ,@(cl-loop for i from 1 to 7
                                            collect `(holiday-fixed 10 ,i "National Day"))
				 (holiday-fixed 10 24 "Programmers' Day")
				 (holiday-fixed 11 11 "Singles' Day")))
  ;; 分别是世界地球日、世界读书日、俄罗斯的那个程序员节
  (setq holiday-other-holidays '((holiday-fixed 4 22 "Earth Day")
				 (holiday-fixed 4 23 "World Book Day")
				 (holiday-sexp '(if (or (zerop (% year 400))
							(and (% year 100) (zerop (% year 4))))
                                                    (list 9 12 year)
                                                  (list 9 13 year))
                                               "World Programmers' Day")))
  (setq calendar-chinese-all-holidays-flag t))

(provide 'ask-emacs)
;;; ask-emacs.el ends here
