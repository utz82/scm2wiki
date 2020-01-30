;; (c) 2020 Michael Neidel
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


;;; # SCM-SEMANTICS
;;;
;;; Destructure Chicken Scheme source code into an s-expression representing the
;;; source on a semantic level.
;;;
;;; scm-semantics recognizes the following elements:
;;;
;;; keyword                    | meaning
;;; ---------------------------|---------------------------------------
;;; `source`                   | Scheme source code
;;; `+- comment`               | stand-alone comment block
;;; `+- constant-definition`   | constant definition using `define-constant`
;;; `  +- comment`             | comment describing the constant
;;; `  +- name`                | name of the constant
;;; `  +- type-annotation`     | type annotation of the constant
;;; `  +- value`               | value of the constant
;;; `+- module-declaration`    | Chicken module declaration
;;; `  +- body`                | body of the module
;;; `  +- exported-symbols`    | list of symbols exported by the module
;;; `  +- name`                | module identifier
;;; `+- procedure-definition`  | procedure definition using `define`
;;; `  +- body`                | body of the procedure definition
;;; `  +- comment`             | comment describing the procedure
;;; `  +- name`                | name of the procedure
;;; `  +- signature`           | signature of the procedure
;;; `  +- type-annotation`     | type annotation of the procedure
;;; `+- record-definition`     | record type definition (chicken/srfi-9/defstruct)
;;; `  +- comment`             | comment describing the record type
;;; `  +- constructor`         | record constructor procedure
;;; `  +- fields`              | all fields of the record type
;;; `    +- field`             | a single field of the record type
;;; `      +- default`         | default field value (defstruct types only)
;;; `      +- getter`          | getter of the field
;;; `      +- setter`          | setter of the field
;;; `      +- type-annotation` | type annotation of the field
;;; `  +- name`                | name of the record type
;;; `  +- predicate`           | record type predicate
;;; `  +- implementation`      | record implementation
;;; `+- syntax-definition`     | syntax (macro) definition
;;; `  +- comment`             | comment describing the macro
;;; `  +- name`                | name of the macro
;;; `  +- signature`           | macro signature
;;; `  +- transformer`         | transformer expression of the macro
;;; `+- variable-definition`   | generic variable definition using `define`
;;; `  +- comment`             | comment describing the variable
;;; `  +- name`                | name of the variable
;;; `  +- type-annotation`     | type annotation of the variable
;;; `  +- value`               | value of the variable
;;;
;;; Only comments starting with a specific prefix are considered. By default,
;;; that prefix is `;;;`.

(module scm-semantics
    ()
  (import scheme (chicken base) (chicken module)
	  (only scm-semantics-impl parse-semantics))

  (reexport scm-semantics-impl)

  ) ;; end module scm-semantics
