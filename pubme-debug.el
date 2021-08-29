;;; https://orgmode.org/worg/exporters/filter-markup.html
;;; Used for testing debug
(require 'org)
(require 'ox-html)

(defun pubme-debug-filter-body
  (text back-end info)
  (format "	$body$%s	$/body$" text))
(defun pubme-debug-filter-bold
  (text back-end info)
  "Markup TEXT as 	$bold$TEXT	$/bold$. Ignore BACK-END and INFO."
  (format "	$bold$%s	$/bold$" text))
(defun pubme-debug-filter-babel-call
  (text back-end info)
  (format "	$bbl$%s	$/bbl$" text))
(defun pubme-debug-filter-center-block
  (text back-end info)
  (format "	$cnt$%s	$/cnt$" text))
(defun pubme-debug-filter-clock
  (text back-end info)
  (format "	$clck$%s	$/clck$" text))
(defun pubme-debug-filter-code
  (text back-end info)
  (format "	$code$%s	$/code$" text))
(defun pubme-debug-filter-comment
  (text back-end info)
  (format "	$cmmn$%s	$/cmmn$" text))
(defun pubme-debug-filter-comment-block
  (text back-end info)
  (format "	$cmm$%s	$/cmm$" text))
(defun pubme-debug-filter-diary-sexp
  (text back-end info)
  (format "	$dry$%s	$/dry$" text))
(defun pubme-debug-filter-drawer
  (text back-end info)
  (format "	$drwr$%s	$/drwr$" text))
(defun pubme-debug-filter-dynamic-block
  (text back-end info)
  (format "	$dyn$%s	$/dyn$" text))
(defun pubme-debug-filter-entity
  (text back-end info)
  (format "	$entt$%s	$/entt$" text))
(defun pubme-debug-filter-example-block
  (text back-end info)
  (format "	$exm$%s	$/exm$" text))
(defun pubme-debug-filter-export-block
  (text back-end info)
  (format "	$exprt-b$%s	$/exprt-b$" text))
(defun pubme-debug-filter-export-snippet
  (text back-end info)
  (format "	$exprt-s$%s	$/exprt-s$" text))
(defun pubme-debug-filter-final-output
  (text back-end info)
  (format "	$fnl$%s	$/fnl$" text))
(defun pubme-debug-filter-fixed-width
  (text back-end info)
  (format "	$fxd$%s	$/fxd$" text))
(defun pubme-debug-filter-footnote-definition
  (text back-end info)
  (format "	$ftnt-d$%s	$/ftnt-d$" text))
(defun pubme-debug-filter-footnote-reference
  (text back-end info)
  (format "	$ftnt-r$%s	$/ftnt-r$" text))
(defun pubme-debug-filter-headline
  (text back-end info)
  (format "	$hdln$%s	$/hdln$" text))
(defun pubme-debug-filter-horizontal-rule
  (text back-end info)
  (format "	$hrz$%s	$/hrz$" text))
(defun pubme-debug-filter-inline-babel-call
  (text back-end info)
  (format "	$inln-b$%s	$/inln-b$" text))
(defun pubme-debug-filter-inline-src-block
  (text back-end info)
  (format "	$inln-s$%s	$/inln-s$" text))
(defun pubme-debug-filter-inlinetask
  (text back-end info)
  (format "	$inln$%s	$/inln$" text))
(defun pubme-debug-filter-italic
  (text back-end info)
  (format "	$itlc$%s	$/itlc$" text))
(defun pubme-debug-filter-item
  (text back-end info)
  (format "	$item$%s	$/item$" text))
(defun pubme-debug-filter-keyword
  (text back-end info)
  (format "	$kywr$%s	$/kywr$" text))
(defun pubme-debug-filter-latex-environment
  (text back-end info)
  (format "	$ltx-n$%s	$/ltx-n$" text))
(defun pubme-debug-filter-latex-fragment
  (text back-end info)
  (format "	$ltx-f$%s	$/ltx-f$" text))
(defun pubme-debug-filter-line-break
  (text back-end info)
  (format "	$ln-b$%s	$/ln-b$" text))
(defun pubme-debug-filter-link
  (text back-end info)
  (format "	$link$%s	$/link$" text))
(defun pubme-debug-filter-node-property
  (text back-end info)
  (format "	$nd-p$%s	$/nd-p$" text))
;; dont (defun pubme-debug-filter-options ...)
(defun pubme-debug-filter-paragraph
  (text back-end info)
  (format "	$prgr$%s	$/prgr$" text))
;; dont (defun pubme-debug-filter-parse-tree ...)
(defun pubme-debug-filter-plain-list
  (text back-end info)
  (format "	$pln-l$%s	$/pln-l$" text))
(defun pubme-debug-filter-plain-text
  (text back-end info)
  (format "	$pln-t$%s	$/pln-t$" text))
(defun pubme-debug-filter-planning
  (text back-end info)
  (format "	$plnn$%s	$/plnn$" text))
(defun pubme-debug-filter-property-drawer
  (text back-end info)
  (format "	$prp$%s	$/prp$" text))
(defun pubme-debug-filter-quote-block
  (text back-end info)
  (format "	$qt-b$%s	$/qt-b$" text))
(defun pubme-debug-filter-radio-target
  (text back-end info)
  (format "	$rd-t$%s	$/rd-t$" text))
(defun pubme-debug-filter-section
  (text back-end info)
  (format "	$sctn$%s	$/sctn$" text))
(defun pubme-debug-filter-special-block
  (text back-end info)
  (format "	$spc$%s	$/spc$" text))
(defun pubme-debug-filter-src-block
  (text back-end info)
  (format "	$src$%s	$/src$" text))
(defun pubme-debug-filter-statistics-cookie
  (text back-end info)
  (format "	$stt$%s	$/stt$" text))
(defun pubme-debug-filter-strike-through
  (text back-end info)
  (format "	$str$%s	$/str$" text))
(defun pubme-debug-filter-subscript
  (text back-end info)
  (format "	$sbsc$%s	$/sbsc$" text))
(defun pubme-debug-filter-superscript
  (text back-end info)
  (format "	$sprs$%s	$/sprs$" text))
(defun pubme-debug-filter-table
  (text back-end info)
  (format "	$tabl$%s	$/tabl$" text))
(defun pubme-debug-filter-table-cell
  (text back-end info)
  (format "	$tbl-c$%s	$/tbl-c$" text))
(defun pubme-debug-filter-table-row
  (text back-end info)
  (format "	$tbl-r$%s	$/tbl-r$" text))
(defun pubme-debug-filter-target
  (text back-end info)
  (format "	$trgt$%s	$/trgt$" text))
(defun pubme-debug-filter-timestamp
  (text back-end info)
  (format "	$tmst$%s	$/tmst$" text))
(defun pubme-debug-filter-underline
  (text back-end info)
  (format "	$undr$%s	$/undr$" text))
(defun pubme-debug-filter-verbatim
  (text back-end info)
  (format "	$vrbt$%s	$/vrbt$" text))
(defun pubme-debug-filter-verse-block
  (text back-end info)
  (format "	$vrs$%s	$/vrs$" text))

(org-export-define-derived-backend 'pubme-html-debug 'html
  :filters-alist
  '((:filter-body . pubme-debug-filter-body)
    (:filter-bold . pubme-debug-filter-bold)
    (:filter-babel-call . pubme-debug-filter-babel-call)
    (:filter-center-block . pubme-debug-filter-center-block)
    (:filter-clock . pubme-debug-filter-clock)
    (:filter-code . pubme-debug-filter-code)
    (:filter-comment . pubme-debug-filter-comment)
    (:filter-comment-block . pubme-debug-filter-comment-block)
    (:filter-diary-sexp . pubme-debug-filter-diary-sexp)
    (:filter-drawer . pubme-debug-filter-drawer)
    (:filter-dynamic-block . pubme-debug-filter-dynamic-block)
    (:filter-entity . pubme-debug-filter-entity)
    (:filter-example-block . pubme-debug-filter-example-block)
    (:filter-export-block . pubme-debug-filter-export-block)
    (:filter-export-snippet . pubme-debug-filter-export-snippet)
    (:filter-final-output . pubme-debug-filter-final-output)
    (:filter-fixed-width . pubme-debug-filter-fixed-width)
    (:filter-footnote-definition . pubme-debug-filter-footnote-definition)
    (:filter-footnote-reference . pubme-debug-filter-footnote-reference)
    (:filter-headline . pubme-debug-filter-headline)
    (:filter-horizontal-rule . pubme-debug-filter-horizontal-rule)
    (:filter-inline-babel-call . pubme-debug-filter-inline-babel-call)
    (:filter-inline-src-block . pubme-debug-filter-inline-src-block)
    (:filter-inlinetask . pubme-debug-filter-inlinetask)
    (:filter-italic . pubme-debug-filter-italic)
    (:filter-item . pubme-debug-filter-item)
    (:filter-keyword . pubme-debug-filter-keyword)
    (:filter-latex-environment . pubme-debug-filter-latex-environment)
    (:filter-latex-fragment . pubme-debug-filter-latex-fragment)
    (:filter-line-break . pubme-debug-filter-line-break)
    (:filter-link . pubme-debug-filter-link)
    (:filter-node-property . pubme-debug-filter-node-property)
    ;;   omit filter with different args
    ;;   (:filter-options . pubme-debug-filter-options)
    (:filter-paragraph . pubme-debug-filter-paragraph)
    ;;   omit filter with different args
    ;;   (:filter-parse-tree . pubme-debug-filter-parse-tree)
    (:filter-plain-list . pubme-debug-filter-plain-list)
    (:filter-plain-text . pubme-debug-filter-plain-text)
    (:filter-planning . pubme-debug-filter-planning)
    (:filter-property-drawer . pubme-debug-filter-property-drawer)
    (:filter-quote-block . pubme-debug-filter-quote-block)
    (:filter-radio-target . pubme-debug-filter-radio-target)
    (:filter-section . pubme-debug-filter-section)
    (:filter-special-block . pubme-debug-filter-special-block)
    (:filter-src-block . pubme-debug-filter-src-block)
    (:filter-statistics-cookie . pubme-debug-filter-statistics-cookie)
    (:filter-strike-through . pubme-debug-filter-strike-through)
    (:filter-subscript . pubme-debug-filter-subscript)
    (:filter-superscript . pubme-debug-filter-superscript)
    (:filter-table . pubme-debug-filter-table)
    (:filter-table-cell . pubme-debug-filter-table-cell)
    (:filter-table-row . pubme-debug-filter-table-row)
    (:filter-target . pubme-debug-filter-target)
    (:filter-timestamp . pubme-debug-filter-timestamp)
    (:filter-underline . pubme-debug-filter-underline)
    (:filter-verbatim . pubme-debug-filter-verbatim)
    (:filter-verse-block . pubme-debug-filter-verse-block)))
