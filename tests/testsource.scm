;; this comment should be ignored

;;; # A Test Source

(module foo
    *

  (import scheme (chicken base) (chicken module) (chicken string)
	  defstruct)

  ;;; A stand-alone comment
  ;;; stretching multiple lines
  ;;;   with extra spaces in front of a line

  ;;; A single-line stand-alone comment

  ;; an undocumented variable definition
  (define bar 0)

  ;;; A very important variable
  (define baz 0)

  ;;; A very important constant
  (define-constant bar1 0)

  ;; An undocumented procedure
  (define (foobar x)
    x)

  ;;; A documented procedure
  (define (bazbar y)
    (+ y y))

  ;;; A record type definition using defstruct
  (defstruct bla x y (z 1))

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

  ) ;; end module foo
