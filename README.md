# scm2wiki

scm2wiki is an in-source documentation tool for [Chicken Scheme](https://call-cc.org/). It generates documentation from comments in a Scheme source file. scm2wiki renders documentation as Github flavored Markdown, as well as Chicken's custom svnwiki format.

### Contents

* [Features](#features)
* [Usage](#usage)
  * [Installation](#installation)
  * [Command Line Usage](#command-line-usage)
  * [Example](#example)
* [Alternatives to scm2wiki](#alternatives-to-scm2wiki)
* [Version History](#version-history)
* [TODO](#todo)
* [Contributing](#contributing)

### Features

scm2wiki uses stand-alone comment blocks and commented definitions in Scheme source code to generate documentation in Markdown or svnwiki format. scm2wiki supports Markdown in comments.

scm2wiki can generate documentation for the following definition types:

- variable and procedure definitions using `define`
- constant definitions using `define-constant`
- macro definitions using `define-syntax` (limited support, see [example](#example))
- record type definitions using `define-record`, `define-record-type`, and `defstruct`
- coops class definitions using `define-class`
- coops generic procedure specializations using `define-method`

scm2wiki understands Chicken Scheme's native `module` declarations. By default, it will only generate documentation for symbols that are exported.

There is also some limited support for simple type annotations, including [typed records](https://wiki.call-cc.org/eggref/5/typed-records). However, this feature is not yet complete.

For svnwiki output, scm2wiki translates Markdown text. However, translation is limited and does not yet support lists, plain hyperlinks, inline images, and formatting in tables.


### Usage

#### Installation

scm2wiki is available from the official Chicken Egg repository. To install it, simply run

```
$ chicken-install scm2wiki
```

#### Command Line Usage

`$ scm2wiki [options...]`

By default, scm2wiki reads from STDIN and outputs Markdown to STDOUT. The following options are available:

option                     | function
---------------------------|-------
`-i`, `--infile=FILENAME`  | specify an input file
`-o`, `--outfile=FILENAME` | specify an output file
`-p`, `--prefix=STRING`    | change the comment prefix (default `";;;"`)
`--toc`                    | automatically generate table of contents
`--svn`                    | output to svnwiki instead of Markdown
`-a`                       | create anchor links (Markdown only)
`--document-internals`     | emit documentation for non-exported symbols

scm2wiki will only consider code comments with a specific, user-defined prefix. By default, the prefix is ";;;". See the following example for a complete list of supported source code elements.

If the `-a` flag is specified, anchor links will be created for each defined binding. This allows for easy cross-referencing of definitions. The anchor id is the name of the binding, prefixed by `def-`. Characters not allowed in URLs are stripped. This feature works only in Markdown mode.


#### Example

````scheme
;; this comment will be ignored

;;; # A Test Heading

(module foo
    *

  (import scheme (chicken base) srfi-9 coops defstruct)

  ;;; A stand-alone comment stretching multiple lines
  ;;; *with* `formatting` **and** a [named link](https://call-cc.org)

  ;;; ```Scheme
  ;;; ;; A fenced code block
  ;;; (+ 1 2 3)
  ;;; ```

  ;;; an   | interesting | table
  ;;; ---- | ----------- | -----
  ;;; with | actual      | content

  ;; An undocumented variable definition
  (define bar 0)

  ;;; A documented variable
  (define baz 0)

  ;;; (procedure (foo X))
  ;;; A manual annotation in the first line of a comment overrides any auto-
  ;;; detected definition type. This is useful to mark closures, which scm2wiki
  ;;; would class as variable definitions otherwise.
  (define foo
    (let ((z #t))
      (lambda (x)
	(set! z x))))

  ;;; Since constants are not visible outside a translation unit, this constant
  ;;; is not included in the documentation.
  ;;; Override this behavior with the `--doc-internals` command line argument.
  (define-constant bar1 0)

  ;; An undocumented procedure
  (define (fooproc x)
    x)

  ;;; A documented procedure
  (define (bazproc y)
    (+ y y))

  ;;; (footax arg1 ...)
  ;;; A list expression at the start of a syntax comment is interpreted as
  ;;; the syntax' signature. scm2wiki does not auto-detect syntax signatures.
  (define-syntax footax
    (syntax-rules ()
      ((_ a ...) (list a ...))))

  ;;; A record type definition using defstruct
  (defstruct bla
    x ;;; a field comment
    y
    (z 1) ;;; another field comment
    )

  ;;; A record type definition using chicken/define-record
  (define-record blu x y)

  ;;; A record type definition using srfi-9
  (define-record-type blorb
    (make-blorb x y)
    blorb?
    (x blorb-x blorb-x-set!)
    (y blorb-y))

  ;;; A very simple coops class type defintion.
  (define-class <myclass> ()
    (foobar))

  ;;; A slightly more complex coops class definition derived from `<myclass>`.
  (define-class <mysubclass> (<myclass>)
    (foo
     (bar 0)
     (baz initform: 0 accessor: myclass-baz)))

  ;;; A procedure specialization on <mysubclass>.
  (define-method (do-foo primary: (baz <mysubclass>) another-arg)
    42)

  ) ;; end module foo
````

The resulting Markdown output:


````markdown
# A Test Heading
## [module] foo

A stand-alone comment stretching multiple lines
*with* `formatting` **and** a [named link](https://call-cc.org)

` ` `Scheme
;; A fenced code block
(+ 1 2 3)
` ` `

an   | interesting | table
---- | ----------- | -----
with | actual      | content

#### [variable] `bar`
**default:** `0`


#### [variable] `baz`
**default:** `0`
A documented variable


#### [procedure] `(foo X)`
A manual annotation in the first line of a comment overrides any auto-
detected definition type. This is useful to mark closures, which scm2wiki
would class as variable definitions otherwise.


#### [procedure] `(fooproc X)`



#### [procedure] `(bazproc Y)`

A documented procedure


#### [syntax] `(footax ARG1 ...)`

A procedure signature at the start of a syntax comment is interpreted as
the syntax' signature. scm2wiki does not auto-detect syntax signatures.


### [record] `bla`
**[constructor] `(make-bla #!key X (Y 1) (Z 1))`**
**[predicate] `bla?`**
**implementation:** `defstruct`

field | getter  | setter       | default | type     | comment
----- | ------- | ------------ | ------- | -------- | ------------------------------
`x`   | `bla-x` | `bla-x-set!` |         |          | `a field comment`
`y`   | `bla-y` | `bla-y-set!` | `1`     |          |
`z`   | `bla-z` | `bla-z-set!` | `1`     | `number` | `a field with type annotation`

A record type definition using defstruct


### [record] `blu`
**[constructor] `(make-blu X Y)`**
**[predicate] `blu?`**
**implementation:** `define-record`

field | getter  | setter
----- | ------- | ------------
`x`   | `blu-x` | `blu-x-set!`
`y`   | `blu-y` | `blu-y-set!`

A record type definition using chicken/define-record


### [record] `blorb`
**[constructor] `(make-blorb X Y)`**
**[predicate] `blorb?`**
**implementation:** `srfi-9`

field | getter    | setter
----- | --------- | --------------
`x`   | `blorb-x` | `blorb-x-set!`
`y`   | `blorb-y` |

A record type definition using srfi-9


### [class] `<myclass>`
slot: `foobar`


A very simple coops class type defintion.


### [class] `<mysubclass>`
**inherits from:** [`<myclass>`](#class-ltmyclassgt)

slot  | initform | accessor
----- | -------- | -------------
`foo` |          |
`bar` | `0`      |
`baz` | `0`      | `myclass-baz`

A slightly more complex coops class definition derived from `<myclass>`.


**[method] `(do-foo primary: (BAZ <mysubclass>) ANOTHER-ARG)`**
A procedure specialization on <mysubclass>.

````

The resulting svnwiki output:

```
== A Test Heading
== Module foo

A stand-alone comment stretching multiple lines
''with'' {{formatting}} '''and''' a [[https://call-cc.org|named link]]

<enscript highlight=#"scheme#">;; A fenced code block
(+ 1 2 3)</enscript>



<table><tr><th>an   </th><th>interesting</th><th>table</th></tr>
<tr><td>with </td><td>actual</td><td>content</td></tr>

</table>

<constant>bar</constant>
; default : {{0}}


<constant>baz</constant>
; default : {{0}}
A documented variable


<procedure>(foo X)</procedure>

A manual annotation in the first line of a comment overrides any auto-
detected definition type. This is useful to mark closures, which scm2wiki
would class as variable definitions otherwise.


<procedure>(fooproc X)</procedure>



<procedure>(bazproc Y)</procedure>

A documented procedure


<syntax>(footax ARG1 ...)</syntax>

A procedure signature at the start of a syntax comment is interpreted as
the syntax' signature. scm2wiki does not auto-detect syntax signatures.


<record>bla</record>
; constructor : {{(make-bla #!key X (Y 1) (Z 1))}}
; predicate : {{bla?}}
; implementation : {{defstruct}}

<table><tr><th>field</th> <th>getter</th> <th>setter</th> <th>default</th> <th>type</th> <th>comment</th></tr><tr><td>{{x}}</td> <td>{{bla-x}}</td> <td>{{bla-x-set!}}</td> <td></td> <td></td> <td>{{a field comment}}</td></tr>
<tr><td>{{y}}</td> <td>{{bla-y}}</td> <td>{{bla-y-set!}}</td> <td>{{1}}</td> <td></td> <td></td></tr>
<tr><td>{{z}}</td> <td>{{bla-z}}</td> <td>{{bla-z-set!}}</td> <td>{{1}}</td> <td>{{number}}</td> <td>{{a field with type annotation}}</td></tr></table>

A record type definition using defstruct


<record>blu</record>
; constructor : {{(make-blu X Y)}}
; predicate : {{blu?}}
; implementation : {{define-record}}

<table><tr><th>field</th> <th>getter</th> <th>setter</th></tr><tr><td>{{x}}</td> <td>{{blu-x}}</td> <td>{{blu-x-set!}}</td></tr>
<tr><td>{{y}}</td> <td>{{blu-y}}</td> <td>{{blu-y-set!}}</td></tr></table>

A record type definition using chicken/define-record


<record>blorb</record>
; constructor : {{(make-blorb X Y)}}
; predicate : {{blorb?}}
; implementation : {{srfi-9}}

<table><tr><th>field</th> <th>getter</th> <th>setter</th></tr><tr><td>{{x}}</td> <td>{{blorb-x}}</td> <td>{{blorb-x-set!}}</td></tr>
<tr><td>{{y}}</td> <td>{{blorb-y}}</td> <td></td></tr></table>

A record type definition using srfi-9


=== Class <myclass>
<class><myclass></class>


; slot : {{foobar}}


A very simple coops class type defintion.



=== Class <mysubclass>
<class><mysubclass></class>

; inherits from : [[#Class-<myclass>|<myclass>]]

<table><tr><th>slot</th> <th>initform</th> <th>accessor</th></tr><tr><td>{{foo}}</td> <td></td> <td></td></tr>
<tr><td>{{bar}}</td> <td>{{0}}</td> <td></td></tr>
<tr><td>{{baz}}</td> <td>{{0}}</td> <td>{{myclass-baz}}</td></tr></table>

A slightly more complex coops class definition derived from {{<myclass>}}.


<method>(do-foo primary: (BAZ <mysubclass>) ANOTHER-ARG)</method>
A procedure specialization on <mysubclass>.
```


### Alternatives to scm2wiki

If you find that scm2wiki isn't suited for your needs, consider one of these alternatives:

- [hahn](https://wiki.call-cc.org/eggref/5/hahn)
- [chalk](https://wiki.call-cc.org/eggref/5/chalk)
- [schematic](https://wiki.call-cc.org/eggref/5/schematic)
- [SchemeDoc](http://people.cs.aau.dk/~normark/schemedoc/)


### Version History

0.4.0 - support for images, plain links, formatted text in tables in svnwiki; automatic TOC generation

0.3.3 - Bugfix: Produce the same level of headers in markdown and svnwiki

0.3.2 - Bugfix: Properly parse strings whose last character is a backslash, don't hide record definitions when their constructor is public

0.3.1 - Bugfix: Parenthesis characters are valid atoms

0.3.0 - Manual annotations for generic definitions, anchor links, various bugfixes

0.2.0 - Support for syntax, records, procedure signatures, coops classes

0.1.0 - initial release


### TODO

* [markdown] transform variadic procedure definitions, svnwiki style
* [svnwiki] support for markdown lists
* ~~[svnwiki] support for images, plain links, formatted text in tables~~ *done*
* [all] better support for type annotations
* [all] better support for macro definitions
* [all] better support for generics
* [all] support parameters
* ~~[all] option to auto-generate TOC~~ *done*
* ~~[markdown] remove unsupported svnwiki tags~~ *done*
* ~~[all] more robust command line arguments parsing~~ *done*
* ~~[all] support for macro, record, constant definitions etc.~~ *done*


### Contributing

Contributions to scm2wiki are very welcome. See the above TODO list for issues that need help.
If you're unsure how to contribute or have any questions, feel free to open an
[issue](https://github.com/utz82/scm2wiki/issues).
