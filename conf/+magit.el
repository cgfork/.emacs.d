;;; package --- Initialize Magit -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (cgfork/try-install 'magit)
  (with-eval-after-load 'magit
    (define-key global-map (kbd "C-x g") 'magit-status)))

(provide '+magit)
;;; +magit.el ends here
