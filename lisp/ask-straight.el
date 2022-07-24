;;; ask-straight.el --- Install straight -*- lexical-binding: t -*-

;; Copyright (C) 2021 cgfork

;; Author: cgfork(cg.fork@gmail.com)
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'url-parse) 
(eval-when-compile (require 'cl-lib))

(defun ask-http-proxy--parse (url)
  (let* ((proxy (url-generic-parse-url url))
	 (http-type (url-type proxy))
	 (http-host (url-host proxy))
	 (http-port (url-port proxy)))
    (when (and http-type http-host http-port)
      `(,http-type ,(format "%s:%s" http-host http-port)))))

;; (setq url-proxy-services
;;       '(("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
;; 	 ("http" . "127.0.0.1:1235")
;; 	 ("https" . "127.0.0.1:1235")))
(defvar ask-proxy-http-url nil)
(defvar ask-proxy-https-url nil)
(defun ask-http-proxy-setup ()
  "Setup the http proxy."
  (when ask-proxy-http-url)
  (let ((proxy-list '())
	(proxy-enabled (or ask-proxy-http-url ask-proxy-https-url)))
    (when ask-proxy-http-url
      (add-to-list 'proxy-list `("http" . ,(cadr (ask-http-proxy--parse ask-proxy-http-url)))))
    (when ask-proxy-https-url
      (add-to-list 'proxy-list `("https" . ,(cadr (ask-http-proxy--parse ask-proxy-https-url)))))
    (when proxy-enabled
      (add-to-list 'proxy-list '("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)"))
      (setq url-proxy-services proxy-list))))

;; setup the proxy before installing the straight
(ask-http-proxy-setup)
(defvar bootstrap-version)
(setq straight-base-dir user-emacs-directory
      straight-cache-autoloads t
      straight-repository-branch "develop"
      straight-check-for-modifications '(check-on-save find-when-checking)
      straight-enable-package-integration nil
      straight-vc-git-default-clone-depth 1
      straight-use-package-by-default t
      use-package-always-ensure nil)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))  
;; Interation with `use-package'
(straight-use-package 'use-package)


(provide 'ask-straight)
;;; ask-straight.el ends here
