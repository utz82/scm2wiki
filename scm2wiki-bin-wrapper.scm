;; scm2wiki binary executable wrapper

;; (c) 2019-2020 Michael Neidel
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

(cond-expand
  (chicken-4 (import chicken scheme)
	     (use args scm2wiki))
  (chicken-5 (import scheme (chicken base) (chicken process-context)
		     (chicken io) (chicken port)
		     args scm-semantics semantics2md)))

(define cmdline-opts
  (list (args:make-option (i infile)
			  #:required "input file name")
	(args:make-option (o outfile)
			  #:required "output file name")
	(args:make-option (p prefix)
			  #:required "documentation comment prefix string")
	(args:make-option (m markdown)
			  #:none "export to Markdown format")
	(args:make-option (h help)
			  #:none "display this text" (usage))))

(define (usage)
  (with-output-to-port (current-error-port)
    (lambda ()
      (print "Usage: " (car (argv)) " -i infile [options...]")
      (newline)
      (print (args:usage cmdline-opts))))
  (exit 1))

;; (receive (options operands)
;;     (args:parse (command-line-arguments)
;; 		cmdline-opts
;; 		#:unrecognized-proc args:ignore-unrecognized-options)
;;   (let ((infile (alist-ref 'i options))
;; 	(outfile (alist-ref 'o options))
;; 	(comment-prefix (alist-ref 'p options))
;; 	(markdown (alist-ref 'm options)))
;;     (if infile
;; 	(s2w:source->doc
;; 	 infile
;; 	 (if outfile
;; 	     outfile
;; 	     (string-append infile (if markdown ".md" ".wiki")))
;; 	 (if comment-prefix comment-prefix s2w:default-prefix)
;; 	 (if markdown 'markdown 'svnwiki))
;; 	(usage))))
