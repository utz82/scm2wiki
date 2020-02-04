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

;;; # SEMANTICS2MD-IMPL
;;; Low-level implementation for semantics2md

(module semantics2md-impl
    *
  (import scheme (chicken base) (chicken module) (chicken string)
	  srfi-1 srfi-13)

  ;; Extract documentation for the aspect given by **aspect-key** from the
  ;; **semantic** source element. Returns an empty string if **semantic** does
  ;; not contain the given aspect.
  (define (aspect->string aspect-key semantic)
    (let ((maybe-doc (alist-ref aspect-key (cdr semantic))))
      (if maybe-doc
	  (car maybe-doc)
	  "")))

  (define (type-annotation->pre definition)
    (if (alist-ref 'type-annotation (cdr definition))
	(string-append " <i>"
		       (car (alist-ref 'type (alist-ref 'type-annotation
							(cdr definition))))
		       "</i>")
	""))

  (define (transform-generic-definition d)
    (string-append "### "
		   (aspect->string 'name d)
		   "\n<pre>"
		   (if (eqv? 'constant-definition (car d))
		       "[CONSTANT]"
		       "[VARIABLE]")
		   " <b>"
		   (aspect->string 'name d)
		   "</b>"
		   (type-annotation->pre d)
		   "</pre>\n"
		   (aspect->string 'comment d)))

  (define (transform-module-declaration d document-internals)
    '())

  ;; TODO type
  (define (transform-procedure-definition d)
    (string-append "### "
		   (aspect->string 'name d)
		   "\n<pre>[PROCEDURE] (<b>"
		   (aspect->string 'name d)
		   "</b> "
		   (aspect->string 'signature d)
		   ")"
		   (type-annotation->pre d)
		   "</pre>\n"
		   (aspect->string 'comment d)))

  (define (transform-record-fields record-definition)
    (let* ((fields (car (alist-ref 'fields record-definition)) )
	   (have-feature (lambda (feature)
			   (any (lambda (f)
				  (alist-ref feature (cdr f)))
				fields))))
      '()))

  ;; TODO ...
  (define (transform-record-definition d)
    (string-append "### "
		   (aspect->string 'name d)
		   "\n<pre>[RECORD] <b>"
		   (aspect->string 'name d)
		   "</b><br>"
		   "[CONSTRUCTOR] "
		   (aspect->string 'constructor d)
		   "<br>[PREDICATE] "
		   (aspect->string 'predicate d)
		   "</pre>\n**FIELDS**\n"
		   ;; ...
		   (aspect->string 'comment d)
		   ))

  (define (transform-syntax-definition d)
    '())

  (define (transform-source-element source-element document-internals)
    (case (car source-element)
      ((comment) (cadr source-element))
      ((constant-definition) (transform-generic-definition source-element))
      ((module-declaration) '())
      ((procedure-definition) '())
      ((record-definition) '())
      ((syntax-definition) '())
      ((variable-definition) (transform-generic-definition source-element))
      (else (error (string-append "Unsupported source element "
				  (->string (car source-element)))))))

  ;;; Generate documentation in Markdown format from  a semantic **source**
  ;;; expression (as produced by parse-semantics from the scm-semantics module).
  ;;; If the source contains a module declaration, only exported symbols will be
  ;;; included in the resulting documentation, unless **document-internals** is
  ;;; set to `#t`.
  (define (semantics->md source #!optional document-internals)
    (when (eqv? 'source (car source))
      (string-intersperse
       (flatten (map (lambda (elem)
		       (transform-source-element elem document-internals))
		     (cdr source)))
       "\n\n")))

  ) ;; end module semantics2md-impl
