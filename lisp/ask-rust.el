;;; ask-rust.el --- Rust -*- lexical-binding: t -*-

;; Copyright (C) 2021 cgfork

;; Author: cgfork(cg.fork@gmail.com)
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:
;; Installation
(unless ask-start-emacs-without-packages
  (use-package rustic)
  (use-package cargo)
  (use-package ob-rust))

;; Configuration
(unless ask-start-emacs-without-packages
  (add-to-list 'auto-mode-alist '("\.rust\\'" . rustic-mode))
  (with-eval-after-load 'rustic
    (setq rustic-lsp-client 'eglot)
    (setq rustic-format-on-save t)
    (add-hook 'rustic-mode-hook 'eglot-ensure)
    (add-hook 'rustic-mode-hook (lambda ()
				  (setq tab-width 4
					standard-indent 2
					indent-tabs-mode nil)))
    (defun rustic-to-fix-with-quit ()
      "Once https://github.com/brotzeit/rustic/issues/253 has been resovled this should 
no longer be necessary."
      (when buffer-file-name
	(setq-local buffer-save-without-query t)))
    (add-hook 'rustic-mode-hook 'rustic-to-fix-with-quit)  
     (with-eval-after-load 'org   
       (add-to-list 'org-src-lang-modes '("rust" . rustic))       
       (org-babel-do-load-languages 'org-babel-load-languages
				    (append org-babel-load-languages
					    '((rust . t)))))))

(provide 'ask-rust)
;;; ask-rust.el ends here
