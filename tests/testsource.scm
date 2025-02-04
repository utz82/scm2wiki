;; this comment will be ignored

;;; # A Test Heading

(module foo
    *

  (import scheme (chicken base) srfi-9 coops typed-records)

  ;;; A stand-alone comment stretching multiple lines
  ;;; *with* `formatting` **and** a [named link](https://call-cc.org)

  ;;; ```Scheme
  ;;; ;; A fenced code block
  ;;; (+ 1 2 3)
  ;;; ```

  ;;; an   | interesting | table
  ;;; ---- | ----------- | -----
  ;;; with | actual      | content

  ;; An undocumented variable definition
  (define bar 0)

  ;;; A documented variable
  (define baz 0)

  ;;; (procedure (foo X))
  ;;; A manual annotation in the first line of a comment overrides any auto-
  ;;; detected definition type. This is useful to mark closures, which scm2wiki
  ;;; would class as variable definitions otherwise.
  (define foo
    (let ((z #t))
      (lambda (x)
	(set! z x))))

  ;;; Since constants are not visible outside a translation unit, this constant
  ;;; is not included in the documentation.
  ;;; Override this behavior with the `--doc-internals` command line argument.
  (define-constant bar1 0)

  ;;; Parameters are detected using simple heuristics. For anything more complex
  ;;; than the definition below, use a manual annotation in the form of
  ;;; (parameter p DEFAULT)
  (define p (make-parameter #t))

  ;; An undocumented procedure
  (define (fooproc x)
    x)

  ;;; A documented procedure
  (define (bazproc y)
    (+ y y))

  ;;; (footax arg1 ...)
  ;;; A list expression at the start of a syntax comment is interpreted as
  ;;; the syntax' signature. scm2wiki does not auto-detect syntax signatures.
  (define-syntax footax
    (syntax-rules ()
      ((_ a ...) (list a ...))))

  ;;; A record type definition using defstruct
  (defstruct bla
    x ;;; a field comment
    (y 1)
    ((z 1) : number) ;;; a field with type annotation
    )

  ;;; A record type definition using chicken/define-record
  (define-record blu x y)

  ;;; A record type definition using srfi-9
  (define-record-type blorb
    (make-blorb x y)
    blorb?
    (x blorb-x blorb-x-set!)
    (y blorb-y))

  ;;; A record type definition using strange but legal formatting
  (define-record-type
      blurp
    (make-blurp x
  		y)
    blurp?

    (x
     blurp-x
     )
    (y blurp-y
       blurp-y-set!)

    ) ;; a hidden comment

  ;;; A very simple coops class type defintion.
  (define-class <myclass> ()
    (foobar))

  ;;; A slightly more complex coops class definition derived from `<myclass>`.
  (define-class <mysubclass> (<myclass>)
    (foo
     (bar 0)
     (baz initform: 0 accessor: myclass-baz)))

  ;;; A procedure specialization on <mysubclass>.
  (define-method (do-foo primary: (baz <mysubclass>) another-arg)
    42)

  ) ;; end module foo
