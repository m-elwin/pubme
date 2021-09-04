#!/bin/sh
":"; exec emacs --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; lexical-binding: t; -*-
;; See https://gist.github.com/ctarbide/99b0ac9f7d6bef19cdd3e9f71b4cbcf7 for meaning of the above line. 
;; Basically, This is a shell script that also functions as a valid elisp file
;; This way we can parse all arguments passed to the script and 
;; Find emacs wherever it is on the path rather than a fixed location

;;; Used to publish a directory of org files to html, using my own css template
;;; Tag a headline with :folded: to make it be folded by default
;;; The special block BEGIN_folded creates a folded section

(require 'org)
(require 'ox-html)

(defconst pubme-dir (file-name-directory (if load-in-progress load-file-name (buffer-file-name))) "The installation directory of pubme")

(load-file (concat pubme-dir "/pubme-debug.el"))

;;; We just tweak the final html, since that is easier than figuring out org export
;;; (and I'm not convinced I can change the order of things like I need)
(defun pubme-final-filter
    (text back-end info)
  (with-temp-buffer
    (insert text)
    ; Here we can tweak final html.  No longer needed at the moment
    (buffer-substring-no-properties (point-min) (point-max)) ;converts the buffer back to a string
    )
)

(defun pubme-headline
    (headline contents info)
  (if (member "folded" (org-element-property :tags headline))
      (let* ((level (+ (org-export-get-relative-level headline info)
                       (1- (plist-get info :html-toplevel-hlevel))))
             )
        (format "<details><summary class=\"header-%s\">%s</summary>\n%s</details>"
                level
                (org-element-property :raw-value headline)
                contents
                )
        )
    (org-export-with-backend 'html headline contents info)
    )
  )

(defun pubme-special-block
    (special-block contents info)
  (if (string= "folded" (org-element-property :type special-block))
      (format "<details class=\"folded\"><summary>Details</summary>\n%s</details>"
              contents
              )
    (org-export-with-backend 'html special-block contents info)
    )
  )


(org-export-define-derived-backend 'pubme-html 'html
  :filters-alist
  '((:filter-final-output . pubme-final-filter)
    )
  :translate-alist
  '((headline . pubme-headline)
    (special-block . pubme-special-block)
    )
  )

(defun pubme-publish-to-html (plist filename pub-dir)
  "Publish an org file to a pubme html file. Return output file name."
  (org-publish-org-to 'pubme-html filename ".html" plist pub-dir)
  )

(defun pubme-publish-to-debug (plist filename pub-dir)
  "Publish an org file to a debug html file. Return output file name."
  (org-publish-org-to 'pubme-html-debug filename ".html" plist pub-dir)
  )

(defun pubme (&optional dir publish-to)
  "Publish a directory containing org files.

DIR directory containing org files that should be published as a single website. If omitted, will be the current directory
PUBLISH-TO the backend to use for the html (defaults to pubme-publish-to-html)

"
  (if (not dir) (setq dir default-directory))
  (if (not publish-to) (setq publish-to 'pubme-publish-to-html))
  (setq pub-dir (concat (file-name-as-directory dir) "html"))

  ;; Find all the org files
  ;; For the available options see https://orgmode.org/manual/Publishing-options.html
  (org-publish
   `("main-project"
      :base-directory ,dir
      :publishing-directory ,pub-dir
      :publishing-function ,publish-to
      :recursive t
      :broken-links mark
      :html-doctype "html5"
      :html-html5-fancy t
      :html-head-include-scripts nil
      :html-head-include-default-style nil
      :html-head "<link rel=\"stylesheet\" href=\"pubme.css\" type=\"text/css\"/>"
      :html-postamble t
      :html-postamble-format (("en" "<p><p class=\"outline-2\">Author: %a</p></p>"))
      :html-link-home "index.html"
      :with-sub-superscript nil
      :section-numbers nil
      :with-latex t
      :with-tags not-in-toc
      )
   :force
   )

  (org-publish
   `("project-data"
     :base-directory ,(concat dir "/images")
     :base-extension any
     :publishing-directory ,(concat (file-name-as-directory dir) "html")
     :publishing-function org-publish-attachment
     )
   :force
   )

  (org-publish
   `("pubme-style"
     :base-directory ,(file-name-directory load-file-name)
     :base-extension "css\\|html\\|jpg\\|png\\|svg"
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
  (message "  -d, --debug
              generate markup with debugging information")
  )


;; if running as a script, parse the command line and call the appropriate function
(if noninteractive
    (let ((projdir default-directory))
      (setq make-backup-files nil)
      (while argv
        (let ((option (pop argv)))
          (cond
           ((string= option "--")               nil)
           ((string= option "-h")               (progn (pubme-print-usage) (kill-emacs 0))) 
           ((string= option "--help")           (progn (pubme-print-usage) (kill-emacs 0)))
           ((string= option "-d")               (progn (pubme projdir 'pubme-publish-to-debug) (kill-emacs 0))) 
           ((string= option "--debug")          (progn (pubme projdir 'pubme-publish-to-debug) (kill-emacs 0))) 
           ((not (string-prefix-p option "--")) (setq projdir option))
           )
          )
        )
      (pubme projdir)
      (kill-emacs 0)
    )
  )

