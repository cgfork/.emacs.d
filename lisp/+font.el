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
  (when sys/linux-x-p
    (cond
     ((member "Ubuntu Mono" (font-family-list))
      (set-face-attribute 'default nil :font "Ubuntu Mono-12"))
     ((member "Source Code Pro" (font-family-list))
      (set-face-attribute 'default nil :font "Source Code Pro-12")))
    (when (member "HYKaiTiJ" (font-family-list))
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
	(set-fontset-font
	 (frame-parameter nil 'font)
	 charset
	 (font-spec :name "-unknown-HYKaiTiJ-normal-normal-normal-*-*-*-*-*-*-0-iso10646-1"
		    :weight 'normal
		    :slant 'normal
		    :size 10)))))
  (when sys/mac-x-p
    (cond
     ((member "Monaco" (font-family-list))
      (set-face-attribute 'default nil :font "Monaco-13"))
     ((member "Source Code Pro" (font-family-list))
      (set-face-attribute 'default nil :font "Source Code Pro-13")))
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font
       (frame-parameter nil 'font)
       charset
       (font-spec :family "Pingfang SC")))))

(provide '+font)
;;; +font.el ends here
