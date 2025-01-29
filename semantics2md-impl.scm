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


;;; Low-level implementation for semantics2md
(module semantics2md-impl
    *
  (import scheme (chicken base) (chicken module) (chicken string)
	  srfi-1 srfi-13 srfi-14)

  (define (make-code-block str)
    (string-append "\n\n```Scheme\n" str "\n```\n\n"))

  (define (make-inline-code-block str)
    (string-append "`"
		   (string-translate str #\newline #\space)
		   "`"))

  ;; Extract documentation for the aspect given by ASPECT-KEY from the
  ;; SEMANTIC source element. Returns an empty string if SEMANTIC does
  ;; not contain the given aspect.
  (define (aspect->string aspect-key semantic)
    (or (alist-ref aspect-key (cdr semantic)) ""))

  (define (definition->anchor semantic)
    (string-append "<a id=\"def-"
		   (string-delete (lambda (c) (memq c '(#\< #\> #\# #\% #\")))
				  (alist-ref 'name (cdr semantic)))
		   "\"></a> "))

  (define (transform-comment definition)
    (if (alist-ref 'comment (cdr definition))
		       (string-append (aspect->string 'comment definition)
				      "  \n")
		       ""))

  (define (transform-generic-definition d write-anchors)
    (let ((type-annotation (alist-ref 'type-annotation (cdr d)))
	  (val (aspect->string 'value d)))
      (string-append "#### "
		     (if write-anchors (definition->anchor d) "")
		     (if (eqv? 'constant-definition (car d))
			 "[constant] "
			 "[variable] ")
		     (make-inline-code-block (aspect->string 'name d))
		     "  \n"
		     (if type-annotation
			 (string-append "**type:** "
					(make-inline-code-block
					 (alist-ref 'type
						    (alist-ref 'type-annotation
							       (cdr d))))
					"  \n")
			 "")
		     (if (eqv? 'constant-definition (car d))
			 "**value:** "
			 "**default:** ")
		     (if (> (string-length val) 80)
			 (make-code-block val)
			 (make-inline-code-block val))
		     "  \n"
		     (transform-comment d))))

  (define (transform-procedure-definition d write-anchors)
    (string-append "#### "
		   (if write-anchors (definition->anchor d) "")
		   "[procedure] "
		   (make-inline-code-block (aspect->string 'signature d))
		   "\n"
		   (if (alist-ref 'type-annotation (cdr d))
		       (string-append
			"**type: "
			(make-inline-code-block
			 (alist-ref 'type
				    (alist-ref 'type-annotation (cdr d)))))
		       "")
		   "  \n"
		   (transform-comment d)))

  (define (string-max-lengths rows)
    (map (lambda (pos)
	   (apply max (map (lambda (row)
			     (string-length (list-ref row pos)))
			   rows)))
	 (iota (length (car rows)))))

  (define (make-md-table header contents #!optional (show-header header))
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
				  (let ((astring (aspect->string a c)))
				    (if (string-null? astring)
					""
					(make-inline-code-block astring))))
				aspects))
			 contents))
	   (cell-widths (string-max-lengths (cons md-header md-body))))
      (if (= 1 (length md-header))
	  (string-append (car md-header) ": " (caar md-body) "\n\n")
	  (string-append
	   "\n"
	   (string-intersperse
	    (map (lambda (row)
		   (string-intersperse (map (lambda (cell cell-width)
					      (string-pad-right cell
								cell-width))
					    row cell-widths)
				       " | "))
		 (append (list md-header (map (lambda (cell-width)
						(make-string cell-width #\-))
					      cell-widths))
			 md-body))
	    "\n")
	   "\n"))))

  (define (transform-record-definition d write-anchors)
    (string-append "### "
		   (if write-anchors (definition->anchor d) "")
		   "[record] "
		   (make-inline-code-block (aspect->string 'name d))
		   "  \n**[constructor] "
		   (make-inline-code-block (aspect->string 'constructor d))
		   "**  \n**[predicate] "
		   (make-inline-code-block (aspect->string 'predicate d))
		   "**  \n**implementation:** "
		   (make-inline-code-block (aspect->string 'implementation d))
		   "  \n"
		   (make-md-table '(name getter setter default type comment)
				  (alist-ref 'fields (cdr d))
				  '(field getter setter default type comment))
		   "  \n"
		   (transform-comment d)))

  ;; TODO extract the signature
  (define (transform-syntax-definition d write-anchors)
    (string-append "#### "
		   (if write-anchors (definition->anchor d) "")
		   "[syntax] "
		   (make-inline-code-block (if (alist-ref 'signature (cdr d))
					       (aspect->string 'signature d)
					       (aspect->string 'name d)))
		   "  \n"
		   (transform-comment d)))

  (define (find-class-methods classname methods)
    (filter (lambda (m)
	      (and (alist-ref 'classes (cdr m))
		   (member classname (alist-ref 'classes (cdr m)))))
	    methods))

  (define (transform-method-definition d)
    (string-append "**[method] "
		   (make-inline-code-block (aspect->string 'signature d))
		   "**  \n"
		   (aspect->string 'comment d)
		   "  \n\n"))

  (define (transform-class-definition d write-anchors methods)
    (let ((used-methods (find-class-methods (aspect->string 'name d)
					    methods)))
      (string-append "### "
		     (if write-anchors (definition->anchor d) "")
		     "[class] "
		     (make-inline-code-block (aspect->string 'name d))
		     (if (null? (alist-ref 'superclasses (cdr d)))
			 ""
			 (string-append
			  "  \n**inherits from:** "
			  (string-concatenate
			   (map (lambda (superclass)
				  (string-append
				   "["
				   (make-inline-code-block superclass)
				   "](#class-lt"
				   (string-downcase
				    (string-translate superclass "<>"))
				   "gt)"))
				(alist-ref 'superclasses
					   (cdr d))))))
		     "  \n"
		     (make-md-table '(name initform accessor getter setter)
				    (alist-ref 'slots (cdr d))
				    '(slot initform accessor getter setter))
		     "\n"
		     (transform-comment d)
		     (or (and (not (null? used-methods))
			      (string-concatenate
			       (cons "  \n\n"
				     (map (lambda (m)
					    (transform-method-definition m))
					  used-methods))))
			 "")
		     "\n")))

  (define (transform-module-declaration
	   d document-internals write-anchors methods)
    (let* ((is-method? (lambda (elem) (eqv? 'method-definition (car elem))))
	   (method-definitions (append methods
				       (filter is-method?
					       (alist-ref 'body (cdr d))))))
      (string-append
       "## [module] "
       (aspect->string 'name d)
       "\n"
       (transform-comment d)
       (string-intersperse
	(map (lambda (elem)
	       (transform-source-element elem
					 document-internals
					 write-anchors
					 method-definitions))
	     (remove
	      is-method?
	      (if document-internals
		  (alist-ref 'body (cdr d))
		  (remove
		   (lambda (def)
		     (or (and (member (car def)
				      '(procedure-definition constant-definition
							     variable-definition
							     class-definition))
			      (not (member (alist-ref 'name (cdr def))
					   (alist-ref 'exported-symbols
						      (cdr d)))))
			 (and (eqv? (car def) 'record-definition)
			      (not (member (string-append
					    "make-"
					    (alist-ref 'name (cdr def)))
					   (alist-ref 'exported-symbols
						      (cdr d)))))))
		   (alist-ref 'body (cdr d))))))
	"\n\n"))))

  (define (transform-source-element source-element
				    document-internals
				    write-anchors
				    #!optional (method-definitions '()))
    (case (car source-element)
      ((comment) (cdr source-element))
      ((constant-definition variable-definition)
       (transform-generic-definition source-element write-anchors))
      ((module-declaration)
       (transform-module-declaration source-element
				     document-internals
				     write-anchors
				     method-definitions))
      ((procedure-definition)
       (transform-procedure-definition source-element write-anchors))
      ((record-definition)
       (transform-record-definition source-element write-anchors))
      ((syntax-definition)
       (transform-syntax-definition source-element write-anchors))
      ((class-definition)
       (transform-class-definition source-element
				   write-anchors
				   method-definitions))
      (else (error (string-append "Unsupported source element "
				  (->string (car source-element)))))))

  (define (generate-toc md-text)
    (let loop ((headings (filter (lambda (s)
				   (and (string-prefix? "#" s)
					(< (string-prefix-length "####" s) 4)))
				 (string-split md-text "\n")))
	       (toc ""))
      (if (null? headings)
	  toc
	  (loop (cdr headings)
		(string-append
		 toc
		 (make-string (* 4 (sub1 (string-prefix-length "###"
							       (car headings))))
			      #\space)
		 "1. "
		 (string-trim (string-trim (car headings)
					   #\#)
			      char-set:blank)
		 "\n")))))

  ;;; Generate documentation in Markdown format from  a semantic SOURCE
  ;;; expression (as produced by parse-semantics from the scm-semantics module).
  ;;; If the source contains a module declaration, only exported symbols will be
  ;;; included in the resulting documentation, unless INTERNALS is `#t`. If
  ;;; ANCHORS is `#t`, manual anchor links will be created for each definition,
  ;;; where the `id` property is the name of the defined binding, prefixed with
  ;;; `-def`.
  (define (semantics->md source #!key internals anchors toc)
    (unless (eqv? 'source (car source))
      (error "Not a semantic source expression."))
    (let* ((is-method? (lambda (elem) (eqv? 'method-definition (car elem))))
	   (method-definitions (filter is-method? (cdr source)))
	   (output
	    (string-append (string-intersperse
			    (map (lambda (elem)
				   (transform-source-element
				    elem internals anchors method-definitions))
				 (remove is-method? (cdr source)))
			    "\n")
			   "\n")))
      (if toc
	  (string-append (generate-toc output) "\n\n" output)
	  output)))

  ) ;; end module semantics2md-impl
