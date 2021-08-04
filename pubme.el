#!/bin/sh
":"; # This is a shell script that also functions as a valid elisp file
":"; # This way we can parse all arguments passed to the script and 
":"; # Find emacs wherever it is on the path rather than a fixed location
":"; # See https://gist.github.com/ctarbide/99b0ac9f7d6bef19cdd3e9f71b4cbcf7 for meaning of the above line
":"; exec emacs --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; lexical-binding: t; -*-


;;; Used to publish a directory of org files to html, using my own css template

(defun pubme (&optional dir)
  "Publish a directory containing org files.

DIR directory containing org files that should be published as a single website. If omitted, will be the current directory

"
  (message "%s" dir)
  )

(defun pubme-disp-help (flag)
  (message "found %s" flag)
  )


(defun pubme-print-usage ()
  (message "Usage: pubme [OPTION]... [project-dir]\n")
  (message "Publishes a collection of org files as a static website\n")
  (message "project-dir is the directory for the org project (defaults to current directory)\n")
  (message "  -h, --help
              display this help message and exit")
  )

;; if running as a script, parse the command line and call the appropriate function
(if noninteractive
    (let ((projdir default-directory))
      (while argv
        (let ((option (pop argv)))
          (cond
           ((string= option "--")               nil)
           ((string= option "-h")               (pubme-print-usage)) 
           ((string= option "--help")           (pubme-print-usage))
           ((not (string-prefix-p option "--")) (setq projdir option))
           )
          )
      )
      (kill-emacs 0)
    )
  )

