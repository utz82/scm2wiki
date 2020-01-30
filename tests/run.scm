(import scheme (chicken base) (chicken io) (chicken string)
	srfi-1 srfi-13 comparse test
	scm-semantics-impl semantics2md-impl)

(test-group
 "Semantic Source Parsing"

 (test-group
  "Parsing S-expressions"

  (test "atoms" '("foo/bar" "|bar baz|" "\"quoted string\"" "#\\("
		  #f #f #f)
	(map (lambda (s)
	       (parse (followed-by (as-string an-atom)
				   end-of-input)
		      s))
	     '("foo/bar" "|bar baz|" "\"quoted string\"" "#\\("
	       "not-end-of-input " "(a-cons)" "not(an-atom")))

  (test "cons-cells" '("()" "(foo)" "(foo (bar (baz)))"
		       "'(foo)" "`(foo ,(bar))" "(foo \n  bar  )"
		       #f #f #f)
	(map (lambda (s)
	       (parse (followed-by (as-string a-cons)
				   end-of-input)
		      s))
	     '("()" "(foo)" "(foo (bar (baz)))"
	       "'(foo)" "`(foo ,(bar))" "(foo \n  bar  )"
	       "not-a-cons" "(not (a cons)" "(not end of input  ) "))))

 (test-group
  "Parsing & Destructuring Definitions"

  (test "parsing generic definitions" '(#t #t #f #f #f)
	(map (lambda (s)
	       (let ((res (parse (followed-by
				  (a-generic-definition ";;;" 'define
							'variable-definition)
				  end-of-input)
				 s)))
		 (if res #t #f)))
	     '("(define foo 1)" ";;; bla\n (define foo 1) \n"
	       "(define (foo x) #t)" "(foo bar baz)"
	       "(define foo (+ 1 1)")))

  (test "destructuring variable definitions"
	'(variable-definition "foo" "(+ 1 1)" "bla")
	(let ((res (parse (a-variable-definition ";;;")
			  ";;; bla\n(define foo (+ 1 1))")))
	  (list (car res)
		(car (alist-ref 'name (cdr res)))
		(car (alist-ref 'value (cdr res)))
		(car (alist-ref 'comment (cdr res))))))

  (test "destructuring procedure signatures"
	'("foo" "(foo x y #!optional (z 1))")
	(parse a-signature "(foo x y #!optional (z 1))"))

  (test "destructuring procedure definitions"
	'(procedure-definition "foo" "A procedure" "(foo x y)" "(+ x y)")
	(let ((res (parse (a-procedure-definition ";;;")
			  ";;; A procedure\n (define (foo x y) (+ x y))\n")))
	  (list (car res)
		(car (alist-ref 'name (cdr res)))
		(car (alist-ref 'comment (cdr res)))
		(car (alist-ref 'signature (cdr res)))
		(car (alist-ref 'body (cdr res))))))
  ))

(test-exit)
