;;; +protobuf.el --- protobuf -*- lexical-binding: t -*-

;; Copyright (C) 2021 cgfork

;; Author: cgfork
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.3"))

;; This file is not part of GNU Emacs.

;;; Commentary:
;;; Code:
(use-package protobuf-mode
  :hook (protobuf-mode . (lambda ()
			   (c-add-style
			    "ewx-style"
			    '((c-basic-offset . 4)
			      (indent-tabs-mode . nil))
			    t))))

(provide '+protobuf)
;;; +protobuf.el ends here
