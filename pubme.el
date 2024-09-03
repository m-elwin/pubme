#!/bin/sh
":"; exec emacs -q --no-site-file -no-splash -no-x-resources --script "$0" -- "$@" # -*- mode: emacs-lisp; lexical-binding: t; -*-
;; See https://gist.github.com/ctarbide/99b0ac9f7d6bef19cdd3e9f71b4cbcf7 for meaning of the above line. 
;; Basically, This is a shell script that also functions as a valid elisp file
;; This way we can parse all arguments passed to the script and 
;; Find emacs wherever it is on the path rather than a fixed location

;;; Used to publish a directory of org files to html, using my own css template
;;; Tag a headline with :folded: to make it be folded by default
;;; The special block BEGIN_folded creates a folded block
;;; Putting #+HEADER: :folded before a code block will cause that code block to be folded
;;;   - Due to the current implementation of pubme, the :folded property must be on its own #+HEADER: line
;;; Use the #+GIT-PUBLISH-URL: to setup a remote git repository where the html is to be published (for use with static site hosting on e.g., github)
;;;    The output files will be placed in a git repository, with a remote at the GIT-PUBLISH-URL
;;;    when remote publishing, the current contents will be force-pushed to the remote
;;; The string ${pubme.BASE_DIR} will be expanded to a relative path pointing to the base directory of exported files
;;;    This feature was added as the easiest way to point to the sylesheet and make the home and up directories work when using
;;;    nested org files, without needing to make boilerplate template code
;;; It can be escaped with \${pubme.BASE_DIR} (or \\ when using an elisp string)
;;; This also can use ORG-BABEL, with langagues specified below (currently just python)
;;; Other
;;; TODO: make this into its own literate document publishable with pubme
;;; TODO: make it easier to use this within emacs

(require 'cmake-mode)

(require 'org)
(require 'ox-html)
(require 'package)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t))
 )
(setq org-confirm-babel-evaluate nil)
(setq org-export-use-babel t)
(setq org-babel-python-command "python3")
(setq org-list-allow-alphabetical 1)
;;; We don't want this setting when loading in interactive mode (e.g., if
;;; this script is loaded in init.el
;;; this setting stops emacs from guessing indentation
(if noninteractive
    (setq python-indent-guess-indent-offset nil)
  )


(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))


(package-initialize)

;;; Allow us to apply css to syntax highlighting
(unless (package-installed-p 'htmlize)
  (if (yes-or-no-p "Install htmlize for syntax highlighting in code blocks? ")
      (progn
        (package-install 'htmlize)
        )
    )
  )

;; fancier bibliographies
(unless (package-installed-p 'citeproc)
  (if (yes-or-no-p "Install citeproc for citations? ")
      (progn
        (package-install 'citeproc)
        )
    )
  )

(setq org-html-htmlize-output-type 'css)

(defconst pubme-dir (file-name-directory (if load-in-progress load-file-name (buffer-file-name))) "The installation directory of pubme")
(defvar pubme-git-publish-url nil "Location where html files should be pushed when publishing")
(defvar pubme-base-dir nil "Base directory of the website to publish")

(load-file (concat pubme-dir "/pubme-debug.el"))

(defun relative-dirs
  (base child)
  """ Get the relative path between the base and child directories, as a series of . values.
      This function simply counts the number of forward slashes and outputs the appropriate number of ../
      We already assume that child is a child of the base directory.
  """
  (if base
      (let ((count (- (length (split-string (expand-file-name child) "/")) (length (split-string (expand-file-name base) "/"))))
            (relpath "."))
        (dotimes (i count)
          (setq relpath (concat relpath "/.."))
          )
        relpath
        )
    ".") ; nil base means we are in the current directory
  )

(defun pubme-macro-expand
    (var exp text)
  """ Replace ${var} with exp in text, and unescape \\${var} as a literal view of ${var} """
  (replace-regexp-in-string (format "[\\]\${%s}" var) (format "${%s}" var)
                            (replace-regexp-in-string (format "\\([^\\]\\)\${%s}" var) (format "\\1%s" exp) text))
  )

;;; We just tweak the final html, since that is easier than figuring out org export
;;; (and I'm not convinced I can change the order of things like I need)
(defun pubme-final-filter
    (text back-end info)
  (with-temp-buffer
    ;; Here we can tweak final html.
    ;; First step is we do our macro replacement

    (insert (pubme-macro-expand "pubme.BASE_DIR" (relative-dirs pubme-base-dir (plist-get info :input-file)) text))
    (buffer-substring-no-properties (point-min) (point-max)) ;converts the buffer back to a string
    )
)


(defun pubme-headline
    (headline contents info)
  """ Translate headlines into HTML """
  (if (member "folded" (org-element-property :tags headline))
      (let* ((level (+ (org-export-get-relative-level headline info)
                       (1- (plist-get info :html-toplevel-hlevel))))
             )
        (format "<details id=\"%s\"><summary class=\"header-%s\">%s</summary>\n%s</details>"
                (org-export-get-reference headline info) ; we add the unique anchor link id to the top-level details to make anchor links work
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
  """ Translate BEGIN_folded blocks into HTML """
  (if (string= "folded" (org-element-property :type special-block))
      (format "<details class=\"folded\"><summary>Details</summary>\n%s</details>"
              contents
              )
    (org-export-with-backend 'html special-block contents info)
    )
  )

(defun pubme-src-block
    (src-block contents info)
  """ Check for special properties of source code blocks """
  (if (member ":folded" (org-element-property :header src-block))
      (format "<details class=\"folded\"><summary>Code</summary>\n%s</details>"
              (org-export-with-backend 'html src-block contents info)
              )
    (org-export-with-backend 'html src-block contents info)
    )
  )

(defun pubme-options-filter
    (exp-plist backend)
  """ Translate some PUBME-specific options into html """
  ;; if the git-publish-url is set, set the pubme-git-publish-url appropriately
  ;; After everything is published, this will be used to publish the url
  (let ((publish-url (plist-get exp-plist :git-publish-url)))
    (if (and pubme-git-publish-url publish-url)
        (progn
          (message "Can only set one GIT-PUBLISH-URL, but found two: %s and %s"
                   publish-url pubme-git-publish-url)
          (kill-emacs 1)
          )
          (if publish-url (setq pubme-git-publish-url publish-url))
      )
    )
  exp-plist
  )

;;;#autoload
(defun pubme-export-as-html
    (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (org-export-to-buffer 'pubme-html "*Pubme HTML Export*"
    async subtreep visible-only body-only ext-plist (lambda () ())))

(org-export-define-derived-backend 'pubme-html 'html
  :filters-alist
  '((:filter-final-output . pubme-final-filter)
    (:filter-options . pubme-options-filter)
    )
  :translate-alist
  '((headline . pubme-headline)
    (special-block . pubme-special-block)
    (src-block . pubme-src-block)
    )
  ; options-alist sets some options to the values we want for the html5 export
  :options-alist
  '((:git-publish-url "GIT-PUBLISH-URL" "git-publish-url" nil) ; url to publish html files via git
    (:html-doctype "HTML_DOCTYPE" nil "html5") ; use html5
    (:html-html5-fancy nil "html5-fancy" t)    ; use html5 tags
    (:html-head-include-scripts nil "html-scripts" nil)  ; no standard org-mode scripts
    (:html-head-include-default-style nil "html-style" nil) ; no default style
    (:html-head   ; header to include the pubme css stylesheet
     "HTML_HEAD"
     nil
     "<link rel=\"stylesheet\" href=\"${pubme.BASE_DIR}/pubme.css\" type=\"text/css\"/>")
    (:html-postamble nil "html-postamble" t) ; do generate a post amble
    (:html-postamble-format nil nil '(("en" "<p><p class=\"outline-2\">Author: %a</p></p>"))) ; include author information
    (:html-link-home "HTML_LINK_HOME" nil "${pubme.BASE_DIR}/index.html") ; home link
    (:html-link-up "HTML_LINK_UP" nil "../index.html")
    (:with-sub-superscript nil "^" nil)
    (:section-numbers nil "num" nil)
    (:with-latex nil "tex" t)
    (:with-tags nil "tags" 'not-in-toc)
    )
  :menu-entry
  '(?p "Export to Pubme"
       ((?H "As HTML buffer" pubme-export-as-html)))
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

return the directory where everything was published
"
  (interactive)
  (if (not dir) (setq dir default-directory))
  (if (not publish-to) (setq publish-to 'pubme-publish-to-html))
  (setq pub-dir (concat (file-name-as-directory dir) "html"))
  (setq pubme-base-dir dir)

  ;; Find all the org files
  ;; For the available options see https://orgmode.org/manual/Publishing-options.html
  (org-publish
   `("main-project"
      :base-directory ,dir
      :publishing-directory ,pub-dir
      :publishing-function ,publish-to
      :exclude "private/.*"
      :recursive t
      :broken-links mark
      )
   :force
   )

  (org-publish
   `("project-data"
     :base-directory ,(concat dir "/images")
     :base-extension any
     :publishing-directory ,(concat (file-name-as-directory dir) "html/images")
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
  (if pubme-git-publish-url
      (progn
        (message "Remote git url for publishing is: %s" pubme-git-publish-url)
        (shell-command (format "git --git-dir %s/.git --work-tree %s init" pub-dir pub-dir))
        (shell-command (format "git --git-dir %s/.git --work-tree %s remote add origin %s" pub-dir pub-dir pubme-git-publish-url))
        (shell-command (format "git --git-dir %s/.git --work-tree %s add ." pub-dir pub-dir))
        (shell-command (format "git --git-dir %s/.git --work-tree %s commit -m 'website'" pub-dir pub-dir))
        )
    )
  pub-dir
  )

(defun pubme-git-push-html(giturl &optional dir)
  "Push the html file to a remote git repository

GITURL - the url of the remote git repository
dir - the publishing directory (set to default-directory by default)
"
  (if (not dir) (setq dir default-directory))
  )

(defun pubme-print-usage ()
  (message "Usage: pubme [OPTION]... [project-dir]\n")
  (message "Publishes a collection of org files as a static website\n")
  (message "project-dir is the directory for the org project (defaults to current directory)\n")
  (message "  -h, --help
              display this help message and exit")
  (message "  -d, --debug
              generate markup with debugging information")
  (message " -p, --push
              push the output html to the hosting site. Hosting site is
              specified in index.org with #+GIT-PUBLISH-URL: git_remote_url_here")      
  )


;; if running as a script, parse the command line and call the appropriate function
(if noninteractive
    (let ((projdir default-directory) (git-push nil) (pdir nil))
      (setq make-backup-files nil)
      (while argv
        (let ((option (pop argv)))
          (cond
           ((string= option "--")               nil)
           ((string= option "-h")               (progn (pubme-print-usage) (kill-emacs 0))) 
           ((string= option "--help")           (progn (pubme-print-usage) (kill-emacs 0)))
           ((string= option "-d")               (progn (pubme projdir 'pubme-publish-to-debug) (kill-emacs 0))) 
           ((string= option "--debug")          (progn (pubme projdir 'pubme-publish-to-debug) (kill-emacs 0)))
           ((string= option "-p")               (setq git-push t))
           ((string= option "--push")           (setq git-push t))
           ((not (string-prefix-p option "--")) (setq projdir option))
           )
          )
        )
      (setq pub-dir (pubme projdir))
      (if git-push
          (if pubme-git-publish-url
              (progn
                (message "Pushing to remote at %s" pubme-git-publish-url)
                (shell-command (format "git --git-dir %s/.git --work-tree %s push -f -u origin %s:master" pub-dir pub-dir
                                 (substring
                                  (shell-command-to-string
                                   (format "git --git-dir %s/.git --work-tree %s branch --show-current" pub-dir pub-dir)
                                   )
                                  0 -1))
                         )
                )
            (message "You must specify a #+GIT-PUBLISH-URL: in index.org")
            )
        )
      (kill-emacs 0)
    )
  )

