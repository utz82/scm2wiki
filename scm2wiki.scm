;;; == SCM2WIKI
;;; ## a simple documentation tool for Chicken Scheme
;;;

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

;; TODO: remove/replace [[toc:]] and [[tags: ...] in markdown mode, and/or offer
;;       an option to generate toc manually

(use srfi-1 srfi-13 extras irregex)

(define s2w:default-prefix ";;;")
(define s2w:heading-tokens '("==" "#"))

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
				 (values (irregex "({{.+?}})")
					 "**" "**" '(or "{{" "}}"))
				 (values (irregex "(\\*\\*.+?\\*\\*)")
					 "{{" "}}" '("**")))
			     (if (eq? mode 'markdown)
				 (values (irregex "(''.+?'')")
					 "*" "*" '("''"))
				 (values (irregex "(\\*.+?\\*)")
					 "''" "''" '("*"))))))
	     (irregex-replace/all
	      search-expr s
	      (lambda (m)
		(string-append prefix (irregex-replace/all
				       strip-expr (irregex-match-substring m))
			       postfix)))))))
    (replace-fmt-type 'italic (replace-fmt-type 'bold str))))

;;; convert svnwiki hyperlinks to markdown, eg.
;;; [[#test]]
;;; [[#test|my test]]
;;; [[https://call-cc.org]]
;;; [[https://call-cc.org|Chicken Scheme]]
(define (s2w:format-links-svn->md str)
  (irregex-replace/all
   ;; (\[\[(#|((htt|ft)ps?:\/\/))[^\]]+\]\])
   '(: "[[" (or "#" (: (or "htt" "ft") "p" (? "s") "://"))
       (*? nonl) "]]")
   str
   (lambda (m)
     (let ((matchstr (string-split
		      (string-drop-right
		       (string-drop (irregex-match-substring m)
				    2)
		       2)
		      "|")))
       (if (> (length matchstr) 1)
	   (string-append "[" (cadr matchstr) "](" (car matchstr) ")")
	   (if (string-prefix? "#" (car matchstr))
	       (string-append "[" (string-drop (car matchstr) 1)
			      "](" (car matchstr) ")")
	       (car matchstr)))))))

;;; convert named markdown hyperlinks to svn
;;; [my test link](#test)
;;; [Chicken Scheme](https://call-cc.org)
(define (s2w:format-named-links-md->svn str)
  (irregex-replace/all
   ;; (\[.*?\]\((#|((htt|ft)ps?:\/\/))\S+?\))
   '(: "[" (*? nonl) "](" (or "#" (: (or "htt" "ft") "p" (? "s") "://"))
       (*? (~ whitespace)) ")")
   str
   (lambda (m)
     (let ((matchstr (string->list (irregex-match-substring m))))
       (string-append "[["
		      (list->string (drop (drop-while (lambda (c)
							(not (equal? c #\()))
						      (drop-right matchstr 1))
					  1))
		      "|"
		      (list->string (take-while (lambda (c)
						  (not (equal? c #\])))
						(drop matchstr 1)))
		      "]]")))))

;;; convert anonymous markdown hyperlinks to svn
;;; https://call-cc.org
(define (s2w:format-anon-hyperlinks-md->svn str)
  (irregex-replace/all
   (irregex "((?<![[(])(htt|ft)ps?://\\S+)")
   str
   (lambda (m)
     (string-append "[[" (irregex-match-substring m) "]]"))))

;;; reformat hyperlinks
(define (s2w:reformat-hyperlinks str mode)
  (if (eq? mode 'markdown)
      (s2w:format-links-svn->md str)
      (s2w:format-named-links-md->svn
       (s2w:format-anon-hyperlinks-md->svn str))))

;;; Check if a source line is a procedure definition
(define (s2w:proc-define? line)
  (string-prefix-ci? "(define (" line))

;;; Transform a heading node into svnwiki/markdown text.
(define (s2w:transform-heading node mode)
  (let* ((heading-char? (lambda (char)
			  (or (equal? char #\=)
			      (equal? char #\#))))
	 (heading (string-drop (cadr node)
			       (if (string-prefix? "==" (cadr node))
				   2 1)))
	 (level (+ 1 (length (take-while heading-char?
					 (string->list heading))))))
    (list (list->string
	   (append (if (eq? mode 'markdown)
		       (make-list level #\#)
		       (make-list (+ 1 level) #\=))
		   (drop-while heading-char? (string->list heading))))
	  "")))

;;; Transform a text node into svnwiki/markdown text.
(define (s2w:transform-text node mode)
  (append (map (lambda (line)
		 (s2w:reformat-hyperlinks (s2w:reformat line mode) mode))
	       (cadr node))
	  (list "")))

;;; Transform a procedure definition node into svnwiki/markdown text.
(define (s2w:transform-definition node mode)
  (if (eq? mode 'markdown)
      (list "**PROCEDURE:**" "```scheme" (cadr node) "```")
      (list (string-append "<procedure>" (cadr node) "</procedure>"))))

;;; Transform a code example node into svnwiki/markdown text.
;;; Example:
;;; <enscript highlight="scheme">
;;; (cons a '(b c))
;;; </enscript>
;;; ```scheme
;;; (cons d '(e f))
;;; ```
(define (s2w:transform-codeblock node mode)
  (let ((lines (drop-right (cdr (caadr node)) 1))
	(language (cadadr node)))
    (if (eq? mode 'markdown)
	(cons (string-append "```" language)
	      (append lines (list "```" "")))
	(cons (string-append "<enscript highlight=\"" language "\">")
	      (append lines (list "</enscript>" ""))))))

;;; Transform meta-nodes into svnwiki/markdown output.
(define (s2w:transform-meta-nodes meta-nodes mode)
  (concatenate (map (lambda (meta-node)
		      (concatenate (map (lambda (node)
					  ((car node) node mode))
					meta-node)))
		    meta-nodes)))

(define (s2w:heading? str prefix)
  (irregex-search `(: bos ,prefix (*? whitespace) (or "#" "=="))
		  str))

(define (s2w:codeblock-begin? str prefix)
  (irregex-search `(: bos ,prefix (*? whitespace) (or "```" "<enscript"))
		  str))

;; generate a s2w:remove-prefix closure for the given prefix
(define (s2w:prefix-remove-proc prefix)
  (lambda (line)
    (s2w:remove-prefix line prefix)))

;;; extract a regular comment block
(define (s2w:extract-regular-comment lines prefix)
  (map (s2w:prefix-remove-proc prefix)
       (take-while (lambda (line)
		     (not (or (s2w:proc-define? line)
			      (s2w:heading? line prefix)
			      (s2w:codeblock-begin? line prefix))))
		   lines)))

;;; extract a code example comment
(define (s2w:extract-code-comment lines prefix)
  (let* ((end-token (if (irregex-search "```" (car lines))
			"```" "</enscript>"))
	 (lang-match (irregex-search (irregex "((?<=(highlight=\"|```))[^\"]+)")
				     (car lines)))
	 (language (if lang-match
		       (string-trim-right (irregex-match-substring lang-match))
		       ""))
	 (comment-head
	  (cons (car lines)
		(take-while (lambda (line)
			      (not (irregex-search end-token line)))
			    (cdr lines)))))
    (list (map (s2w:prefix-remove-proc prefix)
	       (append comment-head
		       (list (car (drop lines (length comment-head))))))
	  language)))

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
		   ((s2w:heading? (car lines) prefix)
		    (list 1 (list s2w:transform-heading
				  (s2w:remove-prefix (car lines)
						     prefix))))
		   ((s2w:codeblock-begin? (car lines) prefix)
		    (let ((block (s2w:extract-code-comment lines prefix)))
		      (list (length (car block))
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

;;; Turn comment blocks into nodes
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

;;; Turn source into a list of meta-nodes, discarding input that will not be
;;; processed
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

;;; Generate a svnwiki or markdown file from the given Scheme source.
;;; If **mode** is 'markdown, output markdown, else svnwiki.
(define (s2w:source->doc infile outfile prefix mode)
  (s2w:export-doc outfile (s2w:parse-source infile prefix mode)))
