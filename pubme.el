#!/usr/bin/emacs --script
;;; Used to publish a directory of org files to html, using my own css template

(defun pubme (&optional dir)
  "Publish a directory containing org files.

DIR directory containing org files that should be published as a single website. If omitted, will be the current directory

"

  )

; if running as a script, parse the command line and call the appropriate function
(if noninteractive
    nil
    )
