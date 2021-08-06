#!/bin/sh
":"; exec emacs --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; lexical-binding: t; -*-
;; See https://gist.github.com/ctarbide/99b0ac9f7d6bef19cdd3e9f71b4cbcf7 for meaning of the above line. 
;; Basically, This is a shell script that also functions as a valid elisp file
;; This way we can parse all arguments passed to the script and 
;; Find emacs wherever it is on the path rather than a fixed location

;;; Used to publish a directory of org files to html, using my own css template

(require 'org)
(require 'ox-html)

;;; 
(defun pubme-body-filter
    (text back-end info)
  ;; Table of contents is wrapped in nav. 
  
  (format "%s</div></div>"
          (replace-regexp-in-string "</nav>"
                                    "</nav><div class=outer><div class=inner>" text))
  )

(org-export-define-derived-backend 'pubme-html 'html
  :filters-alist
  '((:filter-body . pubme-body-filter))
  )

(defun pubme-publish-to-html (plist filename pub-dir)
  "Publish an org file to a pubme html file. Return output file name."
  (org-publish-org-to 'pubme-html filename ".html" plist pub-dir)
  )

(defun pubme (&optional dir)
  "Publish a directory containing org files.

DIR directory containing org files that should be published as a single website. If omitted, will be the current directory

"
  (if (not dir) (setq dir default-directory))
  (message "Publishing org files in %s" dir)
  ;; Find all the org files
  (org-publish
   `("main-project"
      :base-directory ,dir
      :publishing-directory ,(concat (file-name-as-directory dir) "html")
      :publishing-function pubme-publish-to-html
      :recursive t
      :broken-links mark
      :html-doctype "html5"
      :html-html5-fancy t
      :html-head-include-scripts nil
      :html-head-include-default-style nil
      :html-head "<link rel=\"stylesheet\" href=\"pubme.css\" type=\"text/css\"/>"
      )
   :force
   )
  (org-publish
   `("pubme-style"
     :base-directory ,(file-name-directory load-file-name)
     :base-extension "css\\|html\\|jpg\\|png"
     :publishing-directory ,(concat (file-name-as-directory dir) "html")
     :publishing-function org-publish-attachment
     )
   :force
   )

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
      (setq make-backup-files nil)
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
      (pubme projdir)
      (kill-emacs 0)
    )
  )

