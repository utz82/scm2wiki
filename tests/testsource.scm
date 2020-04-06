;; this comment will be ignored

;;; # A Test Heading

(module foo
    *

  (import scheme (chicken base) srfi-9 coops defstruct)

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

  ;;; Since constants are not visible outside a translation unit, this constant
  ;;; is not included in the documentation.
  ;;; Override this behavior with the `--doc-internals` command line argument.
  (define-constant bar1 0)

  ;; An undocumented procedure
  (define (foobar x)
    x)

  ;;; A documented procedure
  (define (bazbar y)
    (+ y y))

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
