;; (c) 2020 Michael Neidel
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.


;;; Low level implementation for semantics2svn
(module semantics2svn-impl
    *
  (import scheme (chicken base) (chicken module) (chicken string)
	  srfi-1 srfi-13 srfi-14 comparse)

  (define char-set:horizontal (char-set-union char-set:graphic char-set:blank))

  (define maybe-whitespace (zero-or-more (in char-set:whitespace)))

  (define horizontal-whitespace (one-or-more (in char-set:blank)))

  (define (print-chars-w/o . args)
    (char-set-difference char-set:printing
			 (apply char-set args)))

  (define a-string
    (sequence (is #\")
	      (zero-or-more (any-of (char-seq "\\\"")
				    (in (print-chars-w/o #\"))))
	      (is #\")))

  (define md-heading
    (sequence* ((leader (one-or-more (is #\#)))
		(gap horizontal-whitespace))
	       (result (list->string (append (make-list (+ 1 (length leader))
							#\=)
					     gap)))))

  (define md-code-block
    (sequence* ((_ (any-of (char-seq "````")
			   (char-seq "```")))
		(language (zero-or-more (in char-set:letter+digit)))
		(_ (is #\newline))
		(code (one-or-more (followed-by (in char-set:printing)
						(none-of (char-seq "```")))))
		(_ (in char-set:printing))
		(_ (any-of (char-seq "````")
			   (char-seq "```"))))
	       (result (string-append
			"<enscript"
			(if (null? language)
			    ">"
			    (string-append " highlight=#\""
					   (string-downcase
					    (list->string language))
					   "#\">"))
			(list->string code)
			"</enscript>"))))

  (define md-table-row
    (sequence* ((_ (maybe (is #\|)))
		(first (as-string (one-or-more
				   (in (print-chars-w/o #\| #\newline)))))
		(_ (is #\|))
		(rest (zero-or-more
		       (sequence* ((arg (as-string
					 (one-or-more (in (print-chars-w/o
							   #\| #\newline)))))
				   (_ (maybe (is #\|))))
				  (result (string-trim-both arg)))))
		(_ (any-of (is #\newline)
			   end-of-input)))
	       (result (cons first rest))))

  (define md-table
    (sequence* ((header md-table-row)
		(_ (one-or-more (in (char-set #\| #\- #\: #\space))))
		(_ (is #\newline))
		(body (one-or-more md-table-row)))
	       (result (string-append
			"\n\n<table><tr>"
			(string-concatenate (map (cute string-append
						   "<th>" <> "</th>")
						 header))
			"</tr>\n"
			(string-concatenate
			 (map (lambda (tr)
				(string-append
				 "<tr>"
				 (string-concatenate (map (cute string-append
							    "<td>" <> "</td>")
							  tr))
				 "</tr>\n"))
			      body))
			"\n</table>"))))

  (define md-link
    (sequence* ((_ (is #\[))
		(name (as-string (one-or-more (in (print-chars-w/o #\])))))
		(_ (char-seq "]("))
		(address (as-string (one-or-more (in (print-chars-w/o #\))))))
		(_ (is #\))))
	       (result (string-append "[[" address "|" name "]]"))))

  (define md-bold
    (sequence* ((_ (char-seq "**"))
		(y (one-or-more (in (print-chars-w/o #\* #\newline))))
		(_ (char-seq "**")))
	       (result (string-append "'''" (list->string y) "'''"))))

  (define md-inline-code
    (sequence* ((_ (is #\`))
		(y (one-or-more (in (print-chars-w/o #\` #\newline))))
		(_ (is #\`)))
	       (result (string-append "{{" (list->string y) "}}"))))

  (define md-italic
    (sequence* ((_ (is #\*))
		(y (one-or-more (in (print-chars-w/o #\* #\newline))))
		(_ (is #\*)))
	       (result (string-append "''" (list->string y) "''"))))

  (define md-text
    (followed-by (zero-or-more (any-of md-heading
				       md-code-block
				       md-table
				       md-link
				       md-bold
				       md-italic
				       md-inline-code
				       a-string
				       (as-string (in char-set:printing))))
		 end-of-input))

  (define (md->svn str)
    (string-concatenate (parse md-text str)))

  (define (svn-make-code-block str)
    (string-append "<enscript highlight=\"scheme\">" str "</enscript>"))

  (define (svn-make-inline-code-block str)
    (string-append "{{" (string-translate str "\n") "}}"))

  ;; Extract documentation for the aspect given by ASPECT-KEY from the
  ;; SEMANTIC source element. Returns an empty string if SEMANTIC does
  ;; not contain the given aspect.
  (define (svn-aspect->string aspect-key semantic)
    (or (alist-ref aspect-key (cdr semantic)) ""))

  (define (svn-transform-comment definition)
    (if (alist-ref 'comment (cdr definition))
	(string-append (md->svn (svn-aspect->string 'comment definition))
		       "\n")
	""))

  (define (svn-transform-generic-definition d)
    (let ((type-annotation (alist-ref 'type-annotation (cdr d)))
	  (val (svn-aspect->string 'value d)))
      (string-append "<constant>"
		     (svn-aspect->string 'name d)
		     "</constant>\n"
		     (if type-annotation
			 (string-append "; type : "
					(svn-make-inline-code-block
					 (alist-ref 'type
						    (alist-ref 'type-annotation
							       (cdr d))))
					"\n")
			 "")
		     (if (eqv? 'constant-definition (car d))
			 "; value : "
			 "; default : ")
		     (if (> (string-length val) 80)
			 (svn-make-code-block val)
			 (svn-make-inline-code-block val))
		     "\n"
		     (svn-transform-comment d))))

  (define (svn-transform-procedure-definition d)
    (string-append "<procedure>"
		   (svn-aspect->string 'signature d)
		   "</procedure>\n"
		   (if (alist-ref 'type-annotation (cdr d))
		       (string-append
			"; type : "
			(svn-make-inline-code-block
			 (alist-ref 'type
				    (alist-ref 'type-annotation (cdr d)))))
		       "")
		   "\n"
		   (svn-transform-comment d)))

  (define (svn-string-max-lengths rows)
    (map (lambda (pos)
	   (apply max (map (lambda (row)
			     (string-length (list-ref row pos)))
			   rows)))
	 (iota (length (car rows)))))

  (define (make-svn-table header contents #!optional (show-header header))
    (let* ((aspects (filter (lambda (feature)
			      (any (lambda (c)
				     (alist-ref feature (cdr c)))
				   contents))
			    header))
	   (md-header (filter-map (lambda (actual show)
				    (and (memv actual aspects)
					 (->string show)))
				  header show-header))
	   (md-body (map (lambda (c)
			   (map (lambda (a)
				  (let ((astring (svn-aspect->string a c)))
				    (if (string-null? astring)
					""
					(svn-make-inline-code-block astring))))
				aspects))
			 contents))
	   (cell-widths (svn-string-max-lengths (cons md-header md-body))))
      (if (= 1 (length md-header))
	  (string-append "; " (car md-header) " : " (caar md-body) "\n\n")
	  (string-append
	   "<table><tr>"
	   (string-intersperse
	    (map (lambda (th)
		   (string-append "<th>" th "</th>"))
		 md-header))
	   "</tr>"
	   (string-intersperse
	    (map (lambda (tr)
		   (string-append
		    "<tr>"
		    (string-intersperse
		     (map (cute string-append "<td>" <> "</td>")
			  tr))
		    "</tr>"))
		 md-body)
	    "\n")
	   "</table>\n"))))

  (define (svn-transform-record-definition d)
    (string-append "<record>"
		   (svn-aspect->string 'name d)
		   "</record>"
		   "\n; constructor : "
		   (svn-make-inline-code-block
		    (svn-aspect->string 'constructor d))
		   "\n; predicate : "
		   (svn-make-inline-code-block
		    (svn-aspect->string 'predicate d))
		   "\n; implementation : "
		   (svn-make-inline-code-block
		    (svn-aspect->string 'implementation d))
		   "\n\n"
		   (make-svn-table
		    '(name getter setter default type comment)
		    (alist-ref 'fields (cdr d))
		    '(field getter setter default type comment))
		   "\n"
		   (svn-transform-comment d)))

  ;; TODO extract the signature
  (define (svn-transform-syntax-definition d)
    (string-append "<syntax>"
		   (if (alist-ref 'signature (cdr d))
		       (svn-aspect->string 'signature d)
		       (svn-aspect->string 'name d))
		   "</syntax>"
		   "\n"
		   (svn-transform-comment d)))

  (define (svn-find-class-methods classname methods)
    (filter (lambda (m)
	      (and (alist-ref 'classes (cdr m))
		   (member classname (alist-ref 'classes (cdr m)))))
	    methods))

  (define (svn-transform-method-definition d)
    (string-append "<method>"
		   (svn-aspect->string 'signature d)
		   "</method>"
		   "\n"
		   (svn-transform-comment d)))

  (define (svn-transform-class-definition d methods)
    (let ((used-methods (svn-find-class-methods (svn-aspect->string 'name d)
						methods)))
      (string-append "=== Class "
		     (svn-aspect->string 'name d)
		     "\n<class>"
		     (svn-aspect->string 'name d)
		     "</class>"
		     "\n"
		     (if (null? (alist-ref 'superclasses (cdr d)))
			 ""
			 (string-append
			  "\n; inherits from : "
			  (string-concatenate
			   (map (lambda (superclass)
				  (string-append
				   "[[#Class-"
				   superclass
				   "|"
				   superclass
				   "]]"))
				(alist-ref 'superclasses (cdr d))))))
		     "\n\n"
		     (make-svn-table '(name initform accessor getter setter)
				     (alist-ref 'slots (cdr d))
				     '(slot initform accessor getter setter))
		     "\n"
		     (svn-transform-comment d)
		     (or (and (not (null? used-methods))
			      (string-concatenate
			       (cons "  \n\n"
				     (map (lambda (m)
					    (svn-transform-method-definition m))
					  used-methods))))
			 "")
		     "\n")))

  (define (svn-transform-module-declaration d document-internals methods)
    (let* ((is-method? (lambda (elem) (eqv? 'method-definition (car elem))))
	   (method-definitions (append methods
				       (filter is-method?
					       (alist-ref 'body (cdr d))))))
      (string-append
       "== Module "
       (svn-aspect->string 'name d)
       "\n"
       (svn-transform-comment d)
       (string-intersperse
	(map (lambda (elem)
	       (svn-transform-source-element elem
					     document-internals
					     method-definitions))
	     (remove
	      is-method?
	      (if document-internals
		  (alist-ref 'body (cdr d))
		  (remove
		   (lambda (def)
		     (and (member (car def)
				  '(procedure-definition constant-definition
							 variable-definition
							 class-definition
							 record-definition))
			  (not (member (alist-ref 'name (cdr def))
				       (alist-ref 'exported-symbols (cdr d))))))
		   (alist-ref 'body (cdr d))))))
	"\n\n"))))

  (define (svn-transform-source-element source-element
					document-internals
					#!optional (method-definitions '()))
    (case (car source-element)
      ((comment)
       (md->svn (cdr source-element)))
      ((constant-definition variable-definition)
       (svn-transform-generic-definition source-element))
      ((module-declaration)
       (svn-transform-module-declaration source-element
					 document-internals
					 method-definitions))
      ((procedure-definition)
       (svn-transform-procedure-definition source-element))
      ((record-definition)
       (svn-transform-record-definition source-element))
      ((syntax-definition)
       (svn-transform-syntax-definition source-element))
      ((class-definition)
       (svn-transform-class-definition source-element
				       method-definitions))
      (else (error (string-append "Unsupported source element "
				  (->string (car source-element)))))))

  (define (semantics->svn source #!key internals anchors)
    (unless (eqv? 'source (car source))
      (error "Not a semantic source expression."))
    (let* ((is-method? (lambda (elem) (eqv? 'method-definition (car elem))))
  	   (method-definitions (filter is-method? (cdr source))))
      (string-append ;; "[[toc:]]\n"
		     (string-intersperse
  		      (map (lambda (elem)
  			     (svn-transform-source-element
  			      elem internals method-definitions))
  			   (remove is-method? (cdr source)))
  		      "\n")
  		     "\n")))

  ) ;; end module semantics2svn-impl
