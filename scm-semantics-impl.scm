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
		  (and val (cons sym val)))
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
	       (result `((identifier . ,identifier) (type . ,annotation)))))

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

  (define (transform-arguments args)
    (let ((make-initializer-list-string
	   (lambda (str)
	     (let ((destructured
		    (parse (sequence* ((_ (is #\())
				       (_ maybe-whitespace)
				       (sym (as-string an-atom))
				       (_ maybe-whitespace)
				       (rest (as-string
					      (zero-or-more
					       (sequence a-sexp
							 maybe-whitespace))))
				       (_ (is #\))))
				      (result (cons sym rest)))
			   str)))
	       (string-append "(" (string-upcase (car destructured))
			      " " (cdr destructured) ")")))))
      (string-intersperse
       (map (lambda (arg)
	      (if (parse a-cons arg)
		  (make-initializer-list-string arg)
		  (string-trim-both (if (or (string-prefix? "#" arg)
					    (string-suffix? ":" arg))
					arg
					(string-upcase arg)))))
	    args)
       " ")))

  (define a-signature
    (sequence* ((_ (is #\())
		(name (as-string an-atom))
		(_ maybe-whitespace)
		(args (zero-or-more (as-string (sequence a-sexp
							 maybe-whitespace))))
		(_ (is #\))))
	       (result (cons name
			     (string-append
			      "("
			      name
			      (if (null? args)
				  ""
				  (string-append " "
						 (transform-arguments args)))
			      ")")))))

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
						       (cdr signature)
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
	       (result
		(let* ((raw-signature (and comment (parse a-signature comment)))
		       (signature (and raw-signature (cdr raw-signature)))
		       (actual-comment
			(and comment
			     (or (and signature
				      (string-drop comment
						   (string-length signature)))
				 comment))))
		  (cons 'syntax-definition
			(filter-map-results '(name body signature comment)
					    (list name body signature
						  actual-comment)))))))

  (define a-field-name+default
    (sequence* ((_ (is #\())
		(_ maybe-whitespace)
		(name (as-string an-atom))
		(_ maybe-whitespace)
		(default (as-string a-sexp))
		(_ maybe-whitespace)
		(_ (is #\))))
	       (result `((name . ,name)
			 (default . ,default)))))

  (define (replace-newlines str)
    (string-translate str #\newline #\space))

  ;; name-only, name+default, name+type-anno, name+default+type-anno
  ;; TODO still not handling all comments
  (define (a-record-field comment-prefix)
    (any-of (sequence* ((name (as-string an-atom))
			(_ (zero-or-more (in char-set:blank)))
			(comment (maybe (a-comment comment-prefix))))
		       (result
			(cons 'field
			      (filter-map-results
			       '(name comment)
			       `(,name ,(and comment
					     (replace-newlines comment)))))))
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
			(comment (maybe (a-comment comment-prefix))))
		       (result
			(cons 'field
			      (append (if (pair? name/default)
					  name/default
					  `((name . ,name/default)))
				      (filter-map-results
				       '(type comment)
				       `(,type
					 ,(and comment
					       (replace-newlines comment))))))))
	    (sequence* ((name+default a-field-name+default)
			(_ maybe-whitespace)
			(comment (maybe (a-comment comment-prefix))))
		       (result (cons 'field
				     (if comment
					 (append name+default
						 `((comment .
							    ,(replace-newlines
							      comment))))
					 name+default))))))

  (define (generate-getters+setters fields record-name)
    (map (lambda (field)
	   (append field `((getter .
			    ,(string-append record-name "-"
					    (alist-ref 'name (cdr field))))
			   (setter .
			    ,(string-append record-name "-"
					    (alist-ref 'name (cdr field))
					    "-set!")))))
	 fields))

  (define (generate-defstruct-constructor fields record-name)
    (cons 'constructor
	  (string-append
	   "(make-" record-name " #!key "
	   (string-intersperse
	    (map (lambda (field)
		   (if (alist-ref 'default (cdr field))
		       (string-append "("
				      (string-upcase
				       (alist-ref 'name (cdr field)))
				      " "
				      (alist-ref 'default (cdr field))
				      ")")
		       (string-upcase (alist-ref 'name (cdr field)))))
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
			 (comment . ,comment)
			 (name . ,name)
			 (implementation . ,implementation)
			 ,(constructor-generator args name)
			 (predicate . ,(string-append name "?"))
			 ,(cons 'fields
				(generate-getters+setters args name))))))

  (define (a-defstruct comment-prefix)
    (a-generic-record-definition comment-prefix "defstruct"
				 generate-defstruct-constructor))

  (define (a-define-record comment-prefix)
    (a-generic-record-definition
     comment-prefix "define-record"
     (lambda (args name)
       (cons 'constructor
	     (string-append "(make-" name " "
			    (string-intersperse
			     (map (lambda (id)
				    (string-upcase (alist-ref 'name id)))
				  (map cdr args)))
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
		(constructor a-signature)
		(_ (one-or-more (in char-set:whitespace)))
		(predicate (as-string an-atom))
		(_ (one-or-more (in char-set:whitespace)))
		(fields (one-or-more (a-srfi-9-field comment-prefix)))
		(_ (is #\))))
	       (result (cons 'record-definition
			     (filter-map-results '(name implementation
							constructor predicate
							fields comment)
						 (list name "srfi-9"
						       (cdr constructor)
						       predicate fields
						       comment))))))

  (define (a-record-definition comment-prefix)
    (any-of (a-defstruct comment-prefix)
	    (a-define-record comment-prefix)
	    (a-define-record-type comment-prefix)))

  (define a-superclass-list
    (enclosed-by (is #\()
		 (zero-or-more (sequence* ((sc (as-string an-atom))
					   (_ maybe-whitespace))
					  (result sc)))
		 (is #\))))

  (define (parse-slot-options opts)
    (if (null? opts)
	'()
	(cons (cons (string->symbol (string-drop-right (car opts) 1))
		    (cadr opts))
	      (parse-slot-options (drop opts 2)))))

  (define (a-class-slotspec comment-prefix)
    (any-of (bind (as-string an-atom)
		  (lambda (r)
		    (result `(slot (name . ,r)))))
	    (sequence* ((_ (is #\())
			(name (as-string an-atom))
			(_ (one-or-more (in char-set:whitespace)))
			(initform (as-string a-sexp))
			(_ maybe-whitespace)
			(_ (is #\))))
		       (result `(slot (name . ,name)
				      (initform . ,initform))))
	    (sequence* ((_ (is #\())
			(name (as-string an-atom))
			(_ (one-or-more (in char-set:whitespace)))
			(slot-options (one-or-more
				       (sequence* ((s (as-string a-sexp))
						   (_ maybe-whitespace))
						  (result s))))
			(_ (is #\))))
		       (result (cons 'slot (append `((name . ,name))
						   (parse-slot-options
						    slot-options)))))))

  (define (a-class-definition comment-prefix)
    (sequence* ((comment (maybe (a-comment comment-prefix)))
		(_ (zero-or-more (in char-set:blank)))
		(_ (char-seq "(define-class"))
		(_ maybe-whitespace)
		(name (as-string an-atom))
		(_ maybe-whitespace)
		(superclasses a-superclass-list)
		(_ maybe-whitespace)
		(slots (enclosed-by (is #\()
				    (one-or-more
				     (sequence* ((slot (a-class-slotspec
							comment-prefix))
						 (_ maybe-whitespace))
						(result slot)))
				    (is #\))))
		(_ (is #\))))
	       (result (cons 'class-definition
			     (filter-map-results
			      '(name superclasses slots comment)
			      (list name superclasses slots comment))))))

  (define a-specialized-argument
    (sequence* ((_ (is #\())
		(_ maybe-whitespace)
		(name (as-string an-atom))
		(_ maybe-whitespace)
		(class (as-string an-atom))
		(_ maybe-whitespace)
		(_ (is #\)))
		(_ maybe-whitespace))
	       (result (cons name class))))

  (define a-method-signature
    (sequence* ((_ (is #\())
		(name (as-string an-atom))
		(_ maybe-whitespace)
		(qualifier (maybe (as-string an-atom)))
		(_ maybe-whitespace)
		(specialized-args (zero-or-more a-specialized-argument))
		(rest-args (zero-or-more (as-string a-sexp)))
		(_ (is #\))))
	       (result (list name qualifier specialized-args rest-args))))

  (define (method-signature->string signature)
    (->string (map ->string
		   (filter (lambda (x)
			     (and x (not (null? x))))
			   (cons (car signature)
				 (cons (or (cadr signature)
					   '())
				       (append (map (lambda (x)
						      (list (string-upcase (car x))
							    (cdr x)))
						    (caddr signature))
					       (list (transform-arguments (cadddr signature))))))))))

  (define (a-method-definition comment-prefix)
    (sequence* ((comment (maybe (a-comment comment-prefix)))
		(_ (zero-or-more (in char-set:blank)))
		(_ (char-seq "(define-method"))
		(_ maybe-whitespace)
	        (signature a-method-signature)
		(_ maybe-whitespace)
		(_ (zero-or-more (sequence a-sexp maybe-whitespace)))
		(_ (is #\))))
	       (result (cons 'method-definition
			     (filter-map-results
			      '(name classes qualifier signature comment)
			      (list (car signature)
				    (and (not (null? (caddr signature)))
					 (map cdr (caddr signature)))
				    (cadr signature)
				    (method-signature->string signature)
				    comment))))))

  (define (a-source-element comment-prefix)
    (any-of (a-constant-definition comment-prefix)
	    (a-variable-definition comment-prefix)
	    (a-procedure-definition comment-prefix)
	    (a-record-definition comment-prefix)
	    (a-syntax-definition comment-prefix)
	    (a-class-definition comment-prefix)
	    (a-method-definition comment-prefix)
	    (bind (a-comment comment-prefix)
		  (lambda (r)
		    (result `(comment . ,r))))
	    a-blank-line
	    a-generic-line))

  (define (filter-source-elements source-elements)
    (remove (lambda (e)
	      (memq e '(blank-line generic-line)))
	    source-elements))

  (define (extract-exported-symbols source-elements)
    (filter-map (lambda (e)
		  (and (memv (car e)
			     '(class-definition variable-definition
						procedure-definition
						record-definition
						syntax-definition))
		       (alist-ref 'name (cdr e))))
		source-elements))

  (define an-export-declaration
    (sequence* ((_ (is #\())
		(_ maybe-whitespace)
		(names (zero-or-more (sequence* ((id (as-string an-atom))
						 (_ maybe-whitespace))
						(result id))))
		(_ (is #\))))
	       (result names)))

  ;; TODO reexports
  (define (a-module-declaration comment-prefix)
    (sequence* ((comment (maybe (a-comment ";;;")))
		(_ (zero-or-more (in char-set:blank)))
		(_ (char-seq "(module"))
		(_ (one-or-more (in char-set:whitespace)))
		(name (as-string an-atom))
		(_ (one-or-more (in char-set:whitespace)))
		(exports (any-of (is #\*)
				 an-export-declaration))
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
				    (if (pair? exports)
					exports
					(extract-exported-symbols (cdr body)))
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
