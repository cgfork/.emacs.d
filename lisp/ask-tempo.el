;;; ask-tempo.el --- Templates Configuration -*- lexical-binding: t -*-

;; Copyright (C) 2021 cgfork

;; Author: cgfork
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.3"))

;; This file is not part of GNU Emacs.

;;; Commentary:
;;; Code:
(require 'cl-lib)
(require 'tempo)

(defvar ask-user-name "cgfork")
(defvar ask-user-email "cg.fork@gmail.com")
(defvar ask-lang-script-tags nil
  "Tempo tags for script template.")

(defun ask-lang-bash-script-template (entry)
  "Define a template for initializing a bash script file."
  (let* ((name (car entry))
	 (email (cadr entry)))
    (tempo-define-template
     "bash-script"
     `("#!/usr/bin/env bash" n
       "#================================================================" n
       "# HEADER" n
       "#================================================================" n
       "#% USAGE" n
       "#+    ${SCRIPT_NAME} [OPTIONS]" n
       "#%"  n
       "#% DESCRIPTION" n
       "#%    " n
       "#%"  n
       "#% EXAMPLES" n
       "#%    ${SCRIPT_NAME} -h" n
       "#%" n
       "#================================================================" n
       "#- IMPLEMENTATION" n
       "#-    version     ${SCRIPT_NAME} " (p "Version: " version) n
       "#-    author      "  ,(format "%s(%s)" name email) n
       "#-" n
       "#================================================================" n
       "#  HISTORY" n
       "#     " ,(format-time-string "%Y/%m/%d") " : "  (s version) " : Script creation" n
       "#" n
       "#================================================================" n
       "#  DEBUG OPTION" n
       "#     set -n # Uncomment to check your syntax, without execution." n
       "#     set -x  # Uncomment to debug this shell script." n
       "#" n
       "#================================================================" n
       "# END OF HEADER" n
       "#================================================================" n
       "set -e # exit if a simple command fails" n
       "set -u # error when expanding unset vars" n
       n
       "SCRIPT_HEADSIZE=$(head -200 ${0} |grep -n \"^# END_OF_HEADER\" | cut -f1 -d:)" n
       "SCRIPT_NAME=$(basename \"$0\")" n
       "SCRIPT_DIR=$(cd \"$(dirname \"$0\")\";pwd)" n
       n
       "usage() {" n
       > "head -${SCRIPT_HEADSIZE:-99} ${0} | grep -e \"^#[%+-]\" | sed -e \"s/^#[%+-]//g\" -e \"s/\\${SCRIPT_NAME}/${SCRIPT_NAME}/g\";" n
       "}" n
       n
       "info() {" n
       > "head -${SCRIPT_HEADSIZE:-99} ${0} | grep -e \"^#-\" | sed -e \"s/^#-//g\" -e \"s/\\${SCRIPT_NAME}/${SCRIPT_NAME}/g\";" n
       "}" n
       n
       "getopt_command() {" n
       > "if [[ \"$(uname)\" == \"Darwin\" ]]; then" n
       > >  "echo \"$(brew --prefix gnu-getopt)/bin/getopt\";" n
       > "else" n
       > > "echo \"$(command -v getopt)\";" n
       > "fi" n
       "}" n
       n
       "GETOPT=$(getopt_command)" n
       n
       "ARGS=$(${GETOPT} -n \"$0\" -o \"hv\" -l \"help,version\" -- \"$@\")" n
       "if [ ${?} != 0 ]; then" n
       > "usage" n
       > "exit 1" n
       "fi" n
       n
       "eval set -- \"${ARGS}\"" n
       n
       "while true; do" n
       > "case ${1} in" n
       > > "-h|--help)" n
       > > > "usage;" n
       > > > "exit 1" n
       > > > ";;" n
       > > "-v|--version)" n
       > > > "info;" n
       > > > "exit 1" n
       > > > ";;" n
       > > "--)" n
       > > > "shift;" n
       > > > "break" n
       > > > ";;" n
       > > "*)" n
       > > > "usage;" n
       > > > "exit 1" n
       > > > ";;" n
       > "esac" n
       "done" n
       n
       "for arg in $@" n
       "do" n
       > "echo \"processing $arg\"" n
       "done" n
       )  
     nil
     "Insert a bash script."
     'ask-lang-script-tags)))

(defun ask-lang-org-script-template (entry)
  "Define a template for initializing a org file."
    (let* ((name (car entry))
	   (email (cadr entry)))
      (tempo-define-template
       "org-script"
       `("#+TITLE: " (p "Title: ") n
	 n
	 "#+STARTUP: showall" n
	 "#+STARTUP: noindent" n
	 "#+STARTUP: nonum" n
	 "#+OPTIONS: ^:{}" n
	 "#+DATE: " ,(format-time-string "<%Y-%m-%d %a>") n
	 "#+AUTHOR: " ,(format "%s(%s)" name email) n
	 r > n)
       nil
       "Insert a org file."
       'ask-lang-script-tags)))

(defun ask-lang-elisp-script-template (entry)
  "Define a template for initializing a org file."
    (let* ((name (car entry))
	   (email (cadr entry)))
      (tempo-define-template
       "elisp-script"
       `(";;; " (file-name-nondirectory (buffer-file-name)) " --- " (p "Desc: ") " -*- lexical-binding: t -*-" n
	 n
	 ";; Copyright (C) 2021 " ,(format "%s" name) n
	 n
	 ";; Author: " ,(format "%s(%s)" name email) n
	 ";; Version: 1.0.0" n
	 ";; Package-Requires: ((emacs \"27.1\"))" n
	 n
	 ";; This file is not part of GNU Emacs." n
	 n
	 ";;; Commentary:" n
	 n
	 ";;; Code:"
	 r > n
	 "(provide '" (file-name-nondirectory (file-name-sans-extension (buffer-file-name))) ")" n
	 ";;; " (file-name-nondirectory (buffer-file-name)) " ends here")
       nil
       "Insert a elisp script."
       'ask-lang-script-tags)))

(defun ask-lang-scripts-setup () 
  "Define the templates."
  (let ((entry (list ask-user-name ask-user-email)))
    (ask-lang-bash-script-template entry)
    (ask-lang-elisp-script-template entry)
    (ask-lang-org-script-template entry)
    (tempo-use-tag-list 'ask-lang-script-tags)))

(setq tempo-interactive t)
(add-hook 'after-init-hook #'ask-lang-scripts-setup)

(provide 'ask-tempo)
;;; ask-tempo.el ends here
