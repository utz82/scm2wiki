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

  (define char-set:horizontal (char-set-union char-set:graphic char-set:blank))

  (define maybe-whitespace (zero-or-more (in char-set:whitespace)))

  (define a-blank-line
    (sequence* ((_ (zero-or-more (in char-set:blank)))
		(_ (is #\newline)))
	       (result 'blank-line)))

  (define a-generic-line
    (sequence* ((_ (one-or-more (in char-set:horizontal)))
		(_ (is #\newline)))
	       (result 'generic-line)))

  (define (comment-prefix p)
    (enclosed-by (zero-or-more (in char-set:blank))
		 (char-seq p)
		 (maybe (in char-set:blank))))

  (define (a-comment-line prefix)
    (enclosed-by (sequence (zero-or-more (in char-set:blank))
			   (comment-prefix prefix)
			   (maybe (in char-set:blank)))
		 (as-string (zero-or-more (in char-set:horizontal)))
		 (is #\newline)))

  (define (a-comment prefix)
    (bind (one-or-more (a-comment-line prefix))
	  (lambda (r)
	    (result (list 'comment (string-intersperse r "\n"))))))

  (define a-string
    (sequence (is #\")
	      (zero-or-more (in (char-set-difference char-set:printing
						     (char-set #\"))))
	      (is #\")))

  (define a-piped-symbol
    (sequence (is #\|)
	      (one-or-more (in (char-set-difference char-set:printing
						    (char-set #\|))))
	      (is #\|)))

  (define an-atom
    (any-of a-string a-piped-symbol
	    (one-or-more (in (char-set-difference char-set:graphic
						  (char-set #\( #\)))))))

  (define a-cons
    (recursive-parser (sequence (is #\()
				(one-or-more (any-of an-atom a-cons))
				(is #\)))))

  (define a-sexp (any-of an-atom a-cons))

  (define (a-generic-definition comment-prefix input-symbol result-symbol)
    (sequence* ((comment (maybe (a-comment comment-prefix)))
		(_ (char-seq (string-append "(" (symbol->string input-symbol)
					    " ")))
		(name (as-string an-atom))
		(_ maybe-whitespace)
		(val (as-string a-sexp))
		(_ maybe-whitespace)
		(_ (sequence (is #\))
			     (is #\newline))))
	       (result `(,result-symbol (name ,name)
					(value ,val)
					,comment))))

  (define (a-constant-definition comment-prefix)
    (a-generic-definition comment-prefix 'define-constant 'constant-definition))

  (define (a-variable-definition comment-prefix)
    (a-generic-definition comment-prefix 'define 'variable-definition))

  (define a-signature
    (sequence* ((_ (is #\())
		(name (as-string an-atom))
		(_ maybe-whitespace)
		(rest (as-string (zero-or-more a-sexp)))
		(_ (is #\))))
	       (result (list name
			     (string-append "("
					    (if (string-null? rest)
						name
						(string-append name " " rest))
					    ")")))))

  ;; TODO alternative syntax, define-type
  (define a-type-annotation
    (sequence* ((_ (char-seq "(: "))
		(identifier (as-string an-atom))
		(annotation (as-string a-sexp))
		(_ (is #\))))
	       (result `(type-annotation ,identifier ,annotation))))

  (define (a-procedure-definition comment-prefix)
    (sequence* ((comment (maybe (a-comment comment-prefix)))
		(_ (char-seq "(define "))
		(signature a-signature)
		(_ maybe-whitespace)
		(body (as-string a-sexp))
		(_ maybe-whitespace)
		(_ (sequence (is #\))
			     (is #\newline))))
	       (result `(procedure-definition (name ,(car signature))
					      (signature ,(cadr signature))
					      (body ,body)
					      ,comment))))
  ;; TODO signatures
  (define (a-syntax-definition comment-prefix)
    (sequence* ((comment (maybe (a-comment comment-prefix)))
		(_ (char-seq "(define-syntax "))
		(name (as-string an-atom))
		(_ maybe-whitespace)
		(body (as-string a-sexp))
		(_ (sequence (is #\))
			     maybe-whitespace)))
	       (result `(syntax-definition (name ,name)
					   (body ,body)
					   ,comment))))

  ;; TODO defstruct uses keyword args in ctor
  (define (a-defstruct comment-prefix)
    (sequence* ((comment (maybe (a-comment comment-prefix)))
		;; TODO ws - linebreak may occur directly after "define-record"
		(_ (char-seq "(defstruct "))
		(name (as-string an-atom))
		(_ (one-or-more (in char-set:whitespace)))
		(args (zero-or-more
		       (sequence* ((field (as-string an-atom))
				   (_ maybe-whitespace))
				  (result field))))
		(_ (is #\)))
		(_ maybe-whitespace))
	       (result `(record-definition
			 ,comment (name ,name) (implementation "defstruct")
			 (constructor ,(string-append "(make-" name " "
						      (string-intersperse args)
						      ")"))
			 (predicate ,(string-append "(" name "? x)"))
			 (fields ,(map (lambda (arg)
					 `(field (name ,arg)))
				       args))))))

  (define (a-define-record comment-prefix)
    (sequence* ((comment (maybe (a-comment comment-prefix)))
		;; TODO ws - linebreak may occur directly after "define-record"
		(_ (char-seq "(define-record "))
		(name (as-string an-atom))
		(_ (one-or-more (in char-set:whitespace)))
		(args (zero-or-more
		       (sequence* ((field (as-string an-atom))
				   (_ maybe-whitespace))
				  (result field))))
		(_ (is #\)))
		(_ maybe-whitespace))
	       (result `(record-definition
			 ,comment (name ,name)
			 (implementation "chicken/define-record")
			 (constructor ,(string-append "(make-" name " "
						      (string-intersperse args)
						      ")"))
			 (predicate ,(string-append "(" name "? x)"))
			 (fields ,(map (lambda (arg)
					 `(field (name ,arg)))
				       args))))))

  (define (a-define-record-type comment-prefix)
    (sequence* ((comment (maybe (a-comment comment-prefix)))
		;; TODO ws - linebreak may occur directly after "define-record"
		(_ (char-seq "(define-record-type "))
		(name (as-string an-atom))
		(_ (one-or-more (in char-set:whitespace)))
		(_ (char-seq "(make-"))
		;; TODO
		(_ (is #\))))))

  (define (a-record-definition comment-prefix)
    (any-of (a-defstruct comment-prefix)
	    (a-define-record comment-prefix)
	    (a-define-record-type comment-prefix)))

  (define (source-elements comment-prefix)
    (bind (one-or-more (any-of (a-constant-definition comment-prefix)
			       (a-variable-definition comment-prefix)
			       (a-procedure-definition comment-prefix)
			       (a-record-definition comment-prefix)
			       (a-comment comment-prefix)
			       a-blank-line
			       a-generic-line))
	  (lambda (r)
	    (result (remove (lambda (e)
			      (memq e `(blank-line generic-line)))
			    r)))))

  ;; TODO reexports
  ;; FIXME an-atom parser is too greedy, will accept "foo ." as atom
  (define (a-module-declaration comment-prefix)
    (sequence* ((_ (char-seq "(module"))
		(_ (one-or-more (in char-set:whitespace)))
		(name (as-string an-atom))
		(_ maybe-whitespace)
		(exports (as-string (any-of (is #\*)
					    a-cons)))
		(_ maybe-whitespace)
		(body (source-elements comment-prefix))
		(_ (is #\)))
		(_ (any-of maybe-whitespace a-generic-line)))
	       (result `(module-declaration (name ,name)
					    (exported-symbols ,exports)
					    (body ,body)))))

  ;;; Parse the source code string **source** into an s-expression describing
  ;;; **source**'s semantics. Comments not starting with **comment-prefix** are
  ;;; ignored. If **comment-prefix** is omitted, it defaults to `;;;`.
  (define (parse-semantics source
			   #!optional (comment-prefix default-comment-prefix))
    (parse (bind (followed-by (one-or-more
			       (any-of (a-module-declaration comment-prefix)
				       (source-elements comment-prefix)))
			      (sequence maybe-whitespace end-of-input))
		 (lambda (r)
		   (result (cons 'source r))))
	   source))

  ) ;; end module scm-semantics-impl
