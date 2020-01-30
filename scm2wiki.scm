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

;; TODO: remove/replace [[toc:]] and [[tags: ...] in markdown mode, and/or offer
;;       an option to generate toc manually
;; TODO: only generate markdown, use markdown-svnwiki to generate svnwiki output

;;; # SCM2WIKI
;;; A simple in-source documentation tool for Chicken Scheme
;;;

(module scm2wiki *

  (cond-expand
    (chicken-4 (use srfi-1 srfi-13 srfi-14 extras scm-semantics semantics2md))
    (chicken-5 (import scheme (chicken base) (chicken module) (chicken string)
		       (chicken io) srfi-1 srfi-13 scm-semantics semantics2md)))

  (reexport scm-semantics semantics2md)

  ) ;; end module scm2wiki
