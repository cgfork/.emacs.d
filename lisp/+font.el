;;; +font.el --- Font Setup -*- lexical-binding: t -*-

;; Copyright (C) 2021 cgfork

;; Author: cgfork
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.3"))

;; This file is not part of GNU Emacs.

;;; Commentary:
;;; Code:

;; There are some ways to fetch the attributes of the display.
;; (list (display-pixel-width) (display-pixel-height))
;; (list (display-mm-width) (display-mm-height))
(defun x-display-ppi ()
  "Return the PPI of the display.

The function assumes that the display has the same pixel width in the
horizontal and vertical directions."
  (if (display-graphic-p)
      (let ((mm-h (cl-caddr (assoc 'mm-size (frame-monitor-attributes)))))
	(round (/ (display-pixel-height)
		  (/ mm-h 25.4))))
    (error "Attemp to calculate the dpi of a non-graphic display")))

(when (display-graphic-p)
  ;; Mono 
  (catch 'loop
      (dolist (font '("Ubuntu Mono" "Fira Code" "Consolas" "SF Mono" "Monaco"))
	(when (member font (font-family-list))
	  (set-face-attribute 'default nil :font font :height 150)
	  (throw 'loop t))))
  ;; Unicode
  (catch 'loop
    (dolist (font '("Symbola" "Apple Symbols" "Symbol"))
      (when (member font (font-family-list))
	(set-fontset-font t 'unicode font nil 'prepend)
	(throw 'loop t))))
  ;; Chinese font
  (dolist (font '("WenQuanYi Micro Hei" "Microsoft Yahei" "HYKaiTiJ" "Pingfang SC"))
    (when (member font (font-family-list))
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
	(set-fontset-font
	 (frame-parameter nil 'font)
	 charset
	 (font-spec :family font))))))

(provide '+font)
;;; +font.el ends here
