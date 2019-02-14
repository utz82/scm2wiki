;; scm2wiki binary executable wrapper

;; (c) 2019 utz/irrlicht project
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

(import chicken scheme)
(include "scm2wiki.scm")

(define (arg-error)
  (error "Usage: scm2wiki -i infile [-o outfile] [-p prefix]"))

(define (read-flag-arg flag default-val)
  (if (member flag (argv) string=)
      (cadr (drop-while (lambda (s) (not (string= s flag)))
			(argv)))
      (if (eq? default-val 'error)
	  (arg-error)
	  default-val)))

(define (parse-args)
  (if (< (length (argv)) 3)
      (arg-error)
      (let* ((infile (read-flag-arg "-i" 'error))
	     (outfile (read-flag-arg "-o" (string-append infile ".wiki")))
	     (comment-prefix (read-flag-arg "-p" s2w:default-prefix)))
	(list infile outfile comment-prefix))))

(apply s2w:source->wiki (parse-args))
