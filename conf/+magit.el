;;; package --- Initialize Magit -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(power-emacs-install 'magit)
(define-key global-map (kbd "C-x g") 'magit-status)
(provide '+magit)
;;; +magit.el ends here
