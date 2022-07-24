;;; ask-windows.el --- Windows -*- lexical-binding: t -*-

;; Copyright (C) 2021 cgfork

;; Author: cgfork(cg.fork@gmail.com)
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:
;; see https://github.com/DogLooksGood/window-numbering.el/blob/master/window-numbering.el

;;; Code:

(eval-when-compile (require 'cl-lib))

(defgroup ask-windows nil
  "Numbered window shortcuts"
  :group 'convenience)

(defface ask-windows-number-face
  '()
  "Face used for the number in the mode-line."
  :group 'ask-windows)

(defvar ask-windows-frame-table nil
  "<frame, (windows . numbers)>")

(defun ask-windows-select-by-number (num &optional deleted)
  "Select the window according the given NUM.

Delete the selected windown if the DELETED is provided."
  (interactive "P")
  (let ((windows (car (gethash (selected-frame) ask-windows-frame-table)))
	(window))
    (if (and (>= num 0) (< num 10) (setq window (aref windows num)))
	(if deleted
	    (delete-window window)
	  (select-window window))
      (error "No %s window" num))))

(defun ask-windows-get-number (&optional window)
  "Get the number of the `selected-window' or the given WINDOW if provided."
  (gethash (or window (selected-window))
           (cdr (gethash (selected-frame) ask-windows-frame-table))))

(defun ask-windows-get-number-string (&optional window)
  (let ((s (int-to-string (ask-windows-get-number window))))
    (propertize s 'face 'ask-windows-number-face)))

(defvar ask-windows--windows nil
  "A vector listing the window for each number.")
(defvar ask-windows--numberic-windows
  "A hash map containing each window's number.")
(defvar ask-windows--unused-numbers
  "A list of unused window numbers.")

(defun ask-windows--assign-number (window &optional num)
  "Assign the given WINDOW to the NUM if provided."
  (if num
      (if (aref ask-windows--windows num)
	  (progn
	    (message "Number %s is assigned to another window" num)
	    nil)
	(setf (aref ask-windows--windows num) window)
	(puthash window num ask-windows--numberic-windows)
	(setq ask-windows--unused-numbers (delq num ask-windows--unused-numbers))
	num)
    (when ask-windows--unused-numbers
      (unless (gethash window ask-windows--numberic-windows)
	(let ((num (car ask-windows--unused-numbers)))
	  (ask-windows--assign-number window num))))))

(defun ask-windows--init-unused-numbers (windows)
  (let ((i 9) unused)
    (while (>= i 0)
      (let ((window (aref windows i)))
        (unless window
          (push (% (1+ i) 10) unused)))
      (cl-decf i))
    unused))

(defun ask-windows-refresh-table ()
  "Refresh the `ask-windows-table'."
  (setq ask-windows--windows (make-vector 10 nil)
	ask-windows--numberic-windows (make-hash-table :size 10)
	ask-windows--unused-numbers (ask-windows--init-unused-numbers ask-windows--windows))
   (puthash (selected-frame) (cons ask-windows--windows ask-windows--numberic-windows) ask-windows-frame-table)
   ;; assgin minibuffer-window to 0
   (when (active-minibuffer-window)
     (ask-windows--assign-number (active-minibuffer-window) 0))
   (let ((windows (window-list nil 0 (frame-first-window))))
     (dolist (window windows)
       (ask-windows--assign-number window))))

;; define interactive functions for keymap
(dotimes (i 10)
  (eval `(defun ,(intern (format "select-window-%s" i)) (&optional arg)
           ,(format "Select the window with number %i." i)
           (interactive "P")
           (ask-windows-select-by-number ,i arg))))

 (defvar ask-windows-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "\M-0" 'select-window-0)
    (define-key map "\M-1" 'select-window-1)
    (define-key map "\M-2" 'select-window-2)
    (define-key map "\M-3" 'select-window-3)
    (define-key map "\M-4" 'select-window-4)
    (define-key map "\M-5" 'select-window-5)
    (define-key map "\M-6" 'select-window-6)
    (define-key map "\M-7" 'select-window-7)
    (define-key map "\M-8" 'select-window-8)
    (define-key map "\M-9" 'select-window-9)
    map)
  "Keymap used in by `ask-windows-mode'.")

;;;###autoload
(define-minor-mode ask-windows-mode
  "A minor mode that assigns a number to each window."
  :keymap ask-windows-keymap :global t
  (if ask-windows-mode
      (unless ask-windows-frame-table
        (save-excursion
          (setq ask-windows-frame-table (make-hash-table :size 16))
          (add-hook 'minibuffer-setup-hook 'ask-windows-refresh-table)
          (add-hook 'window-configuration-change-hook
                    'ask-windows-refresh-table)
          (dolist (frame (frame-list))
            (select-frame frame)
            (ask-windows-refresh-table))))
    (ask-windows-clear-mode-line)
    (remove-hook 'minibuffer-setup-hook 'ask-windows-refresh-table)
    (remove-hook 'window-configuration-change-hook
                 'ask-windows-refresh-table)
    (setq ask-windows-frame-table nil)))

(defun ask-windows-clear-mode-line ()
  "Remove the window number from the mode-line."
  (let ((mode-line (default-value 'mode-line-format))
        (res))
    (while mode-line
      (let ((item (car mode-line)))
        (unless (equal item '(:eval (ask-windows-get-number-string)))
          (push item res)))
      (pop mode-line))
    (setq-default mode-line-format (nreverse res)))
  (force-mode-line-update t))


(defun ask-switch-to-active-minibuffer ()
  "Switch to the minibuffer if it is active."
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))

(defun ask-windows-transient-winner-undo ()
  "Use `transient-map' to undo or redo winner."
  (interactive)
  (let ((echo-keystrokes nil))
    (winner-undo)
    (message "Winner: [u]ndo [r]edo")
    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (define-key map [?u] #'winner-undo)
       (define-key map [?r] #'winner-redo)
       map)
     t)))

;; Configurations

(ask-windows-mode t)
(global-set-key (kbd "<f7>") 'ask-switch-to-active-minibuffer)
(add-hook 'after-init-hook #'winner-mode)
(with-eval-after-load 'ediff
  ;; Close the window after ediff quit.
  (add-hook 'ediff-quit-hook #'winner-undo))

(provide 'ask-windows)
;;; ask-windows.el ends here
