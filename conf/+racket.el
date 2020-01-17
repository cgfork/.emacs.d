;;; package --- Initialized Racket -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(cgfork/try-install 'racket-mode)
(cgfork/after-load 'racket-mode
  (cgfork/open-paredit 'racket-mode-hook)
  (add-hook 'racket-mode-hook
	    (lambda ()
	      (define-key racket-mode-map (kbd "<f5") 'racket-run))))

(provide '+racket)
;;; +racket.el ends here
