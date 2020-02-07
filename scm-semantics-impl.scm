;; (c) 2019 Michael Neidel
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

;;; # SCM-SEMANTICS-IMPL
;;; Low-level implementation of scm-semantics

(module scm-semantics-impl
    *
  (import scheme (chicken base) (chicken module) (chicken string)
	  srfi-1 srfi-13 srfi-14 comparse)

  (define-constant default-comment-prefix ";;;")

  (define (filter-map-results symbols results)
    (filter-map (lambda (sym val)
		  (and val (if (pair? val)
			       (cons sym val)
			       (list sym val))))
		symbols results))

  (define char-set:horizontal (char-set-union char-set:graphic char-set:blank))

  (define maybe-whitespace (zero-or-more (in char-set:whitespace)))

  (define a-blank-line
    (sequence* ((_ (zero-or-more (in char-set:blank)))
		(_ (is #\newline)))
	       (result 'blank-line)))

  (define (comment-prefix p)
    (enclosed-by (zero-or-more (in char-set:blank))
		 (char-seq p)
		 (maybe (in char-set:blank))))

  (define (a-comment-line prefix)
    (enclosed-by (sequence (zero-or-more (in char-set:blank))
			   (comment-prefix prefix))
		 (as-string (zero-or-more (in char-set:horizontal)))
		 (is #\newline)))

  ;; TODO multi-line comments with #|...|#
  (define (a-comment prefix)
    (bind (one-or-more (a-comment-line prefix))
	  (lambda (r)
	    (result (string-intersperse r "\n")))))

  (define a-string
    (sequence (is #\")
	      (zero-or-more (any-of (char-seq "\\\"")
				    (in (char-set-difference char-set:printing
							     (char-set #\")))))
	      (is #\")))

  (define a-piped-symbol
    (sequence (is #\|)
	      (one-or-more (in (char-set-difference char-set:printing
						    (char-set #\|))))
	      (is #\|)))

  ;; TODO this is not entirely correct: anything following a ; is not an atom
  (define an-atom
    (any-of a-string a-piped-symbol
	    (sequence (is #\#)
		      (is #\\)
		      (in (char-set #\( #\))))
	    (one-or-more (any-of (in (char-set-difference
				      char-set:graphic (char-set #\( #\))))))))

  ;; TODO quoted symbols, inline comments
  (define a-cons
    (recursive-parser (sequence (maybe (in (char-set #\' #\` #\,)))
				(is #\()
				maybe-whitespace
				(zero-or-more (sequence (any-of an-atom a-cons)
							maybe-whitespace))
				(is #\)))))

  (define a-sexp (any-of a-cons an-atom))

  (define a-generic-line
    (bind (any-of (sequence maybe-whitespace a-cons)
		  (followed-by (sequence maybe-whitespace (is #\;)
					 (zero-or-more
					  (in char-set:horizontal)))
			       (any-of end-of-input (is #\newline))))
	  (lambda (r)
	    (result 'generic-line))))


  ;; TODO alternative syntax, define-type
  (define a-type-annotation
    (sequence* ((_ (char-seq "(: "))
		(identifier (as-string an-atom))
		(_ maybe-whitespace)
		(annotation (as-string a-sexp))
		(_ (is #\)))
		(_ (zero-or-more (in char-set:blank)))
		(_ (is #\newline)))
	       (result `((identifier ,identifier) (type ,annotation)))))

  ;;; TODO output type-annotation
  (define (a-generic-definition comment-prefix input-symbol result-symbol)
    (sequence* ((comment (maybe (a-comment comment-prefix)))
		(_ (zero-or-more (in char-set:blank)))
		(type (maybe a-type-annotation))
		(_ (zero-or-more (in char-set:blank)))
		(_ (char-seq (string-append "(" (symbol->string input-symbol)
					    " ")))
		(name (as-string an-atom))
		(_ maybe-whitespace)
		(val (as-string a-sexp))
		(_ maybe-whitespace)
		(_ (is #\)))
		(_ maybe-whitespace))
	       (result (cons result-symbol
			     (filter-map-results
			      '(name type-annotation value comment)
			      (list name type val comment))))))

  (define (a-constant-definition comment-prefix)
    (a-generic-definition comment-prefix 'define-constant 'constant-definition))

  (define (a-variable-definition comment-prefix)
    (a-generic-definition comment-prefix 'define 'variable-definition))

  (define a-signature
    (sequence* ((_ (is #\())
		(name (as-string an-atom))
		(_ maybe-whitespace)
		(args (as-string (zero-or-more (sequence a-sexp
							 maybe-whitespace))))
		(_ (is #\))))
	       (result (list name (string-append "(" name " " args ")")))))

  (define (a-procedure-definition comment-prefix)
    (sequence* ((comment (maybe (a-comment comment-prefix)))
		(_ (zero-or-more (in char-set:blank)))
		(_ (char-seq "(define"))
		(_ (one-or-more (in char-set:whitespace)))
		(signature a-signature)
		(_ maybe-whitespace)
		(body (as-string (one-or-more (sequence a-sexp
							maybe-whitespace))))
		(_ maybe-whitespace)
		(_ (is #\)))
		(_ maybe-whitespace))
	       (result (cons 'procedure-definition
			     (filter-map-results '(name signature body comment)
						 (list (car signature)
						       (cadr signature)
						       body comment))))))
  ;; TODO signatures
  (define (a-syntax-definition comment-prefix)
    (sequence* ((comment (maybe (a-comment comment-prefix)))
		(_ (zero-or-more (in char-set:blank)))
		(_ (char-seq "(define-syntax"))
		(_ (one-or-more (in char-set:whitespace)))
		(name (as-string an-atom))
		(_ maybe-whitespace)
		(body (as-string a-sexp))
		(_ maybe-whitespace)
		(_ (sequence (is #\))))
		(_ maybe-whitespace))
	       (result (cons 'syntax-definition
			     (filter-map-results '(name body comment)
						 (list name body comment))))))

  (define a-field-name+default
    (sequence* ((_ (is #\())
		(_ maybe-whitespace)
		(name (as-string an-atom))
		(_ maybe-whitespace)
		(default (as-string a-sexp))
		(_ maybe-whitespace)
		(_ (is #\))))
	       (result `((name ,name)
			 (default ,default)))))

  ;; name-only, name+default, name+type-anno, name+default+type-anno
  ;; TODO still not handling all comments
  (define (a-record-field comment-prefix)
    (any-of (sequence* ((name (as-string an-atom))
			(_ (zero-or-more (in char-set:blank)))
			(comment (maybe (a-comment-line ";;;"))))
		       (result (cons 'field
				     (filter-map-results '(name comment)
							 `(,name ,comment)))))
	    (sequence* ((_ (is #\())
			(_ maybe-whitespace)
			(name/default (any-of (as-string an-atom)
					      a-field-name+default))
			(_ (one-or-more (in char-set:whitespace)))
			(_ (is #\:))
			(_ (one-or-more (in char-set:whitespace)))
			(type (as-string a-sexp))
			(_ maybe-whitespace)
			(_ (is #\)))
			(_ maybe-whitespace)
			(comment (maybe (a-comment-line comment-prefix))))
		       (result
			(cons 'field
			      (append (if (pair? name/default)
					  name/default
					  (list (list 'name name/default)))
				      (filter-map-results '(type comment)
							  `(,type ,comment))))))
	    (bind a-field-name+default
		  (lambda (r)
		    (result (cons 'field r))))))

  (define (generate-getters+setters fields record-name)
    (map (lambda (field)
	   (append field `((getter
			    ,(string-append record-name "-"
					    (car (alist-ref 'name
							    (cdr field)))))
			   (setter
			    ,(string-append record-name "-"
					    (car (alist-ref 'name
							    (cdr field)))
					    "-set!")))))
	 fields))

  (define (generate-defstruct-constructor fields record-name)
    (list 'constructor
	  (string-append
	   "(make-" record-name " "
	   (string-intersperse
	    (map (lambda (field)
		   (let ((field-name (car (alist-ref 'name (cdr field)))))
		     (string-append field-name ": " field-name "1")))
		 fields))
	   ")")))

  (define (a-generic-record-definition comment-prefix implementation
				       constructor-generator)
    (sequence* ((comment (maybe (a-comment comment-prefix)))
		(_ (zero-or-more (in char-set:blank)))
		(_ (char-seq (string-append "(" implementation)))
		(_ (one-or-more (in char-set:whitespace)))
		(name (as-string an-atom))
		(_ (one-or-more (in char-set:whitespace)))
		(args (zero-or-more
		       (sequence* ((field (a-record-field comment-prefix))
				   (_ maybe-whitespace))
				  (result field))))
		(_ (is #\)))
		(_ maybe-whitespace))
	       (result `(record-definition
			 (comment,comment)
			 (name ,name)
			 (implementation ,implementation)
			 ,(constructor-generator args name)
			 (predicate ,(string-append "(" name "? x)"))
			 ,(cons 'fields
				(generate-getters+setters args name))))))

  (define (a-defstruct comment-prefix)
    (a-generic-record-definition comment-prefix "defstruct"
				 generate-defstruct-constructor))

  (define (a-define-record comment-prefix)
    (a-generic-record-definition
     comment-prefix "define-record"
     (lambda (args name)
       (list 'constructor
	     (string-append "(make-" name " "
			    (string-intersperse
			     (map (lambda (field)
				    (car (alist-ref 'name (cdr field))))
				  args))
			    ")")))))

  (define (a-srfi-9-field comment-prefix)
    (sequence* ((_ (is #\())
		(_ maybe-whitespace)
		(name (as-string an-atom))
		(_ (one-or-more (in char-set:whitespace)))
		(getter (as-string an-atom))
		(_ maybe-whitespace)
		(setter (maybe (as-string an-atom)))
		(_ maybe-whitespace)
		(type-annotation
		 (maybe (sequence* ((_ (is #\:))
				    (_ (one-or-more (in char-set:whitespace)))
				    (type (as-string a-sexp)))
				   (result type))))
		(_ maybe-whitespace)
		(_ (is #\)))
		(_ maybe-whitespace)
		(comment (maybe (a-comment-line comment-prefix))))
	       (result (cons 'field (filter-map-results
				     '(name getter setter type comment)
				     (list name getter setter type-annotation
					   comment))))))

  (define (a-define-record-type comment-prefix)
    (sequence* ((comment (maybe (a-comment comment-prefix)))
		(_ (zero-or-more (in char-set:blank)))
		(_ (char-seq "(define-record-type"))
		(_ (one-or-more (in char-set:whitespace)))
		(name (as-string an-atom))
		(_ (one-or-more (in char-set:whitespace)))
		(constructor (as-string a-cons))
		(_ (one-or-more (in char-set:whitespace)))
		(predicate (as-string an-atom))
		(_ (one-or-more (in char-set:whitespace)))
		(fields (one-or-more (a-srfi-9-field comment-prefix)))
		(_ (is #\))))
	       (result (cons 'record-definition
			     (filter-map-results '(name implementation
							constructor predicate
							fields comment)
						 (list name "srfi-9" constructor
						       predicate fields
						       comment))))))

  (define (a-record-definition comment-prefix)
    (any-of (a-defstruct comment-prefix)
	    (a-define-record comment-prefix)
	    (a-define-record-type comment-prefix)))

  (define (a-source-element comment-prefix)
    (any-of (a-constant-definition comment-prefix)
	    (a-variable-definition comment-prefix)
	    (a-procedure-definition comment-prefix)
	    (a-record-definition comment-prefix)
	    (a-syntax-definition comment-prefix)
	    (bind (a-comment comment-prefix)
		  (lambda (r)
		    (result `(comment ,r))))
	    a-blank-line
	    a-generic-line))

  (define (filter-source-elements source-elements)
    (remove (lambda (e)
	      (memq e '(blank-line generic-line)))
	    source-elements))

  (define (extract-exported-symbols source-elements)
    (filter-map (lambda (e)
		  (and (memq (car e)
			     '(constant-definition variable-definition
						   procedure-definition
						   record-definition
						   syntax-definition))
		       (car (alist-ref 'name (cdr e)))))
	 source-elements))

  ;; TODO reexports
  (define (a-module-declaration comment-prefix)
    (sequence* ((comment (maybe (a-comment ";;;")))
		(_ (zero-or-more (in char-set:blank)))
		(_ (char-seq "(module"))
		(_ (one-or-more (in char-set:whitespace)))
		(name (as-string an-atom))
		(_ (one-or-more (in char-set:whitespace)))
		(exports (as-string (any-of (is #\*)
					    a-cons)))
		(_ maybe-whitespace)
		(body (bind (one-or-more (a-source-element comment-prefix))
			    (lambda (r)
			      (result (filter-source-elements r)))))
		(_ maybe-whitespace)
		(_ (is #\))))
	       (result (cons 'module-declaration
			     (filter-map-results
			      '(name comment exported-symbols body)
			      (list name comment
				    (if (string= exports "*")
					(extract-exported-symbols body)
					exports)
				    body))))))

  ;;; Parse the source code string **source** into an s-expression describing
  ;;; **source**'s semantics. Comments not starting with **comment-prefix** are
  ;;; ignored. If **comment-prefix** is omitted, it defaults to `;;;`.
  (define (parse-semantics source
			   #!optional (comment-prefix default-comment-prefix))
    (parse (bind (followed-by (one-or-more
			       (any-of (a-module-declaration comment-prefix)
				       (a-source-element comment-prefix)))
			      (sequence maybe-whitespace end-of-input))
		 (lambda (r)
		   (result (cons 'source (filter-source-elements r)))))
	   source))

  ) ;; end module scm-semantics-impl
