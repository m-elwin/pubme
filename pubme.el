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
(message "%s" argv)
; if running as a script, parse the command line and call the appropriate function
(if noninteractive
    (if (elt argv 0)
        (pubme (elt argv 0))
      (pubme default-directory)
      )
  )

(kill-emacs 0)
