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

(import scheme (chicken base) (chicken process-context)
	(chicken io) (chicken port)
	args scm-semantics semantics2md semantics2svn)

(define cmdline-opts
  (list (args:make-option (i infile)
			  #:required "input file name")
	(args:make-option (o outfile)
			  #:required "output file name")
	(args:make-option (a anchors)
			  #:none "create anchor links (markdown only)")
	(args:make-option (p prefix)
			  #:required "documentation comment prefix string")
	(args:make-option (svn)
			  #:none "export to svnwiki format")
	(args:make-option (toc)
			  #:none "generate table of contents (svnwiki only)")
	(args:make-option (doc-internals)
			  #:none "document non-exported symbols in modules")
	(args:make-option (h help)
			  #:none "display this text" (usage))))

(define (usage)
  (with-output-to-port (current-error-port)
    (lambda ()
      (print "Usage: " (car (argv)) " -i infile [options...]")
      (newline)
      (print (args:usage cmdline-opts))))
  (exit 1))

(receive (options operands)
    (args:parse (command-line-arguments)
		cmdline-opts
		#:unrecognized-proc args:ignore-unrecognized-options)
  (let ((infile (alist-ref 'i options))
	(outfile (alist-ref 'o options))
	(comment-prefix (alist-ref 'p options)))
    (write-string
     ((if (alist-ref 'svn options)
	  semantics->svn
	  semantics->md)
      (parse-semantics (read-string #f
				    (or (and infile
					     (open-input-file infile text:))
					(current-input-port)))
		       (or comment-prefix ";;;"))
      internals: (alist-ref 'doc-internals options)
      anchors: (alist-ref 'a options)
      toc: (alist-ref 'toc options))
     #f
     (or (and outfile
	      (open-output-file outfile text:))
	 (current-output-port)))))
