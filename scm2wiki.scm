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


(use srfi-1 srfi-13 extras irregex)

(define s2w:default-prefix ";;;")
(define s2w:heading-tokens '("==" "#"))
(define s2w:codeblock-begin-tokens '("<enscript>" "```"))
(define s2w:codeblock-end-tokens '("</enscript>" "```"))

;; Some internal function that will not appear in the output documentation.
(define (s2w:remove-prefix line prefix)
  (if (string-prefix? prefix line)
      (let ((prefix-len (string-length prefix)))
	(string-drop line (if (> (string-length line) prefix-len)
			      (+ 1 prefix-len)
			      prefix-len)))
      line))

;;; Replace formatted sections of {{str}} with formatting appropriate for
;;; {{mode}}
(define (s2w:reformat str mode)
  (let ((replace-fmt-type
	 (lambda (type s)
	   (let-values (((search-expr prefix postfix strip-expr)
			 (if (eq? type 'bold)
			     (if (eq? mode 'markdown)
				 (values '(: "{{" (+ (~ "}}")) "}}")
					 "**" "**" '(or "{{" "}}"))
				 (values '(: "**" (+ (~ "**")) "**")
					 "{{" "}}" '("**")))
			     (if (eq? mode 'markdown)
				 (values '(: "''" (+ (~ "''")) "''")
					 "*" "*" '("''"))
				 (values '(: "*" (+ (~ "*")) "*")
					 "''" "''" '("*"))))))
	     (irregex-replace/all
	      search-expr s
	      (lambda (m)
		(string-append prefix (irregex-replace/all
				       strip-expr (irregex-match-substring m))
			       postfix)))))))
    (replace-fmt-type 'italic (replace-fmt-type 'bold str))))

;;; Check if a source line is a procedure definition
(define (s2w:proc-define? line)
  (string-prefix-ci? "(define (" line))

(define (s2w:strip-token token-lst line)
  (if (string-prefix? (car token-lst) line)
      (string-drop line (string-length (car token-lst)))
      (s2w:strip-token (cdr token-lst) line)))

(define (s2w:transform-heading node mode)
  (let* ((heading-char? (lambda (char)
			  (or (equal? char #\=)
			      (equal? char #\#))))
	 (heading (s2w:strip-token s2w:heading-tokens (cadr node)))
	 (level (+ 1 (length (take-while heading-char?
					 (string->list heading))))))
    (list (list->string
	   (append (if (eq? mode 'markdown)
		       (make-list level #\#)
		       (make-list (+ 1 level) #\=))
		   (drop-while heading-char? (string->list heading))))
	  "")))

(define (s2w:transform-text node mode)
  (append (map (lambda (line)
		 (s2w:reformat line mode))
	       (cadr node))
	  (list "")))

(define (s2w:transform-definition node mode)
  (if (eq? mode 'markdown)
      (list "**PROCEDURE:**" "```scheme" (cadr node) "```")
      (list (string-append "<procedure>" (cadr node) "</procedure>"))))

(define (s2w:transform-codeblock node mode)
  (let ((lines (cadr node)))
    (if (eq? mode 'markdown)
	(cons "```scheme"
	      (append lines (list "```")))
	(cons "<enscript=scheme>"
	      (append lines (list "</enscript>"))))))

(define (s2w:transform-nodes nodes mode)
  (concatenate (map (lambda (node)
		      ((car node) node mode))
		    nodes)))

(define (s2w:transform-meta-nodes meta-nodes mode)
  (concatenate (map (lambda (meta-node)
		      (s2w:transform-nodes meta-node mode))
		    meta-nodes)))

(define (s2w:any-tokens? token-lst line)
  (not (null? (filter (lambda (token)
			(string-contains line token))
		      token-lst))))

(define (s2w:prefix-remove-proc prefix)
  (lambda (line)
    (s2w:remove-prefix line prefix)))

(define (s2w:extract-regular-comment lines prefix)
  (map (s2w:prefix-remove-proc prefix)
       (take-while (lambda (line)
		     (and (not (s2w:proc-define? line))
			  (not (s2w:any-tokens?
				(append s2w:heading-tokens
					s2w:codeblock-begin-tokens)
				line))))
		   lines)))

(define (s2w:extract-code-comment lines prefix)
  (let ((comment-head
	 (take-while
	  (lambda (line)
	    (not (s2w:any-tokens? s2w:codeblock-end-tokens
				  line)))
	  lines)))
    (map (s2w:prefix-remove-proc prefix)
	 (append comment-head (car (drop lines (length comment-head)))))))

;;; Transform comment blocks into meta-nodes
(define (s2w:get-subnodes lines prefix)
  (if (null? lines)
      '()
      ;; next-node returns number of lines consumed in car, node in cadr
      (let ((next-node
	     (cond ((s2w:proc-define? (car lines))
		    (list 1 (list s2w:transform-definition
				  (s2w:remove-prefix (car lines)
						     "(define"))))
		   ((s2w:any-tokens? s2w:heading-tokens
				     (car lines))
		    (list 1 (list s2w:transform-heading
				  (s2w:remove-prefix (car lines)
						     prefix))))
		   ((s2w:any-tokens? s2w:codeblock-begin-tokens
				     (car lines))
		    (let ((block (s2w:extract-code-comment lines prefix)))
		      (list (length block)
			    (list s2w:transform-codeblock block))))
		   (else (let ((block (s2w:extract-regular-comment
				       lines prefix)))
			   (list (length block)
				 (list s2w:transform-text block)))))))
	(cons (cadr next-node)
	      (s2w:get-subnodes (drop lines (car next-node))
				prefix)))))

;;; Bring procedure definitions to the first line
(define (s2w:reorder-nodes nodes)
  (let ((proc-define? (lambda (node)
			(eq? s2w:transform-definition (car node)))))
    (if (any proc-define? nodes)
	(cons (find proc-define? nodes)
	      (remove proc-define? nodes))
	nodes)))

(define (s2w:block->nodes comment prefix)
  (s2w:reorder-nodes (s2w:get-subnodes comment prefix)))

;;; Extract comment blocks from source lines
(define (s2w:extract-blocks lines)
  (if (null? lines)
      '()
      (let ((next-block
	     (take-while (lambda (s)
			   (if (string-null? (car lines))
			       (string-null? s)
			       (not (string-null? s))))
			 lines)))
	(cons next-block
	      (s2w:extract-blocks
	       (drop lines (length next-block)))))))

;;; Transform source into a list of typed nodes, discarding input that will not
;;; be processed
(define (s2w:nodify lines prefix)
  (map (lambda (block)
	 (s2w:block->nodes block prefix))
       (remove (lambda (block)
		 (or (string-null? (car block))
		     (s2w:proc-define? (car block))))
	       (s2w:extract-blocks (s2w:filter-input lines prefix)))))

;;; Remove leading whitespace and drop all lines but those containing
;;; comments with the desired prefix, or type annotations, or definitions
(define (s2w:filter-input lines prefix)
  (filter (lambda (s)
	    (or (string-null? s)
	        (string-prefix? prefix s)
	        (s2w:proc-define? s)))
	  (map string-trim lines)))

;;; Parse a Scheme source file into a list of meta-nodes.
(define (s2w:parse-source filename prefix mode)
  (s2w:transform-meta-nodes (s2w:nodify (read-lines filename)
					prefix)
			    mode))

;;; Export a list of svnwiki strings to the given file.
(define (s2w:export-doc filename lines)
  (call-with-output-file filename
    (lambda (out)
      (for-each (lambda (line)
		  (write-line line out))
		lines))))

;;; Generate a svnwiki file from the given Scheme source.
;;; If {{prefix}} is omitted, ";;;" will be used.
(define (s2w:source->doc infile outfile prefix mode)
  (s2w:export-doc outfile (s2w:parse-source infile prefix mode)))
