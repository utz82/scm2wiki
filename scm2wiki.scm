;;; == SCM2WIKI
;;; a simple documentation tool for Chicken Scheme
;;;

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


(use srfi-1 srfi-13 extras)

(define s2w:default-prefix ";;;")

;;; Peek ahead in the list of lines to see if the current comment block is
;;; followed by a definition. If it is, the line containing the definition is
;;; returned.
(define (s2w:peek-ahead lines prefix)
  (if (null? lines)
      #f
      (cond ((string-prefix? "<procedure>" (car lines))
	     (car lines))
	    ((not (string-prefix? prefix (car lines)))
	     #f)
	    (else (s2w:peek-ahead (cdr lines) prefix)))))

;;; extract the comment block starting at (car lines)
(define (s2w:get-comment lines prefix)
  (if (or (null? lines)
	  (not (string-prefix? prefix (car lines))))
      '()
      (cons (car lines)
	    (s2w:get-comment (cdr lines) prefix))))

;;; Reduce multiple consecutive empty lines to a single one.
(define (s2w:collapse-empty-lines lines)
  (letrec ((collapse-empty
	     (lambda (remaining-lines previous-line-empty?)
	       (if (null? remaining-lines)
		   '()
		   (if previous-line-empty?
		       (if (string-null? (car remaining-lines))
			   (collapse-empty (cdr remaining-lines) #t)
			   (cons (car remaining-lines)
				 (collapse-empty (cdr remaining-lines) #f)))
		       (cons (car remaining-lines)
			     (collapse-empty (cdr remaining-lines)
					     (string-null?
					      (car remaining-lines)))))))))
    (collapse-empty lines #f)))

;;; Swap defines with their preceding comment
(define (s2w:swap-defines lines prefix)
  (if (null? lines)
      '()
      (let ((comment (s2w:get-comment lines prefix)))
	(if (null? comment)
	    (if (string-prefix? "<procedure>" (car lines))
		(s2w:swap-defines (cdr lines) prefix)
		(cons (car lines)
		      (s2w:swap-defines (cdr lines) prefix)))
	    (let ((next-define (s2w:peek-ahead lines prefix)))
	      (append (if next-define
			  (cons next-define comment)
			  comment)
		      (s2w:swap-defines (drop lines (length comment))
					prefix)))))))

;;; filter out function definitions that are not commented.
(define (s2w:remove-uncommented-defines lines prefix)
  (letrec ((rm-uncommented
	    (lambda (remaining-lines preceded-by-comment?)
	      (if (null? remaining-lines)
		  '()
		  (if (and (not preceded-by-comment?)
			   (string-prefix? "<procedure>"
					   (car remaining-lines)))
		      (rm-uncommented (cdr remaining-lines) #f)
		      (cons (car remaining-lines)
			    (rm-uncommented
			     (cdr remaining-lines)
			     (string-prefix? prefix
					     (car remaining-lines)))))))))
    (rm-uncommented lines #f)))

;; Some internal function that will not appear in the output documentation.
(define (s2w:remove-prefix line prefix)
  (if (string-prefix? prefix line)
      (string-trim (string-drop line (string-length prefix)))
      line))

;;; Convert function definitions to <procedure>(fn args)</procedure> tags.
(define (s2w:tokenize-defines line)
  (if (string-prefix? "(define (" line)
      (string-append "<procedure>"
		     (substring/shared line 8)
		     "</procedure>")
      line))


;;; remove leading whitespace and drop all lines but those containing
;;; comments with the desired prefix, or type annotations, or definitions
(define (s2w:filter-input lines prefix)
  (remove (lambda (s)
	    (and (not (string-null? s))
		 (not (string-prefix? prefix s))
		 (not (string-prefix? "(define (" s))))
	  (map string-trim lines)))


(define (s2w:get-prefix-arg prefix)
  (if (null? prefix)
      s2w:default-prefix
      (car prefix)))

;;; Parse a Scheme source file into a list of svnwiki strings.
(define (s2w:parse-file filename . prefix)
  (let ((pf (s2w:get-prefix-arg prefix)))
    (map (lambda (s)
	   (s2w:remove-prefix s pf))
	 (s2w:swap-defines
	  (s2w:collapse-empty-lines
	   (s2w:remove-uncommented-defines
	    (map s2w:tokenize-defines (s2w:filter-input (read-lines filename)
							pf))
	    pf))
	  pf))))

;;; Export a list of svnwiki strings to the given file.
(define (s2w:export-wiki filename lines)
  (call-with-output-file filename
    (lambda (out)
      (for-each (lambda (line)
		  (write-line line out))
		lines))))

;;; Generate a svnwiki file from the given Scheme source.
;;; If {{prefix}} is omitted, ";;;" will be used.
(define (s2w:source->wiki infile outfile . prefix)
  (let ((pf (s2w:get-prefix-arg prefix)))
    (s2w:export-wiki outfile (s2w:parse-file infile pf))))
