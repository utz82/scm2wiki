# scm2wiki

scm2wiki is a simple in-source documentation tool for Scheme, written in [Chicken Scheme](https://call-cc.org/). It generates documentation from comments in a Scheme source file. scm2wiki can render documentation as (Github flavored) Markdown, as well as Chicken's custom svnwiki format.

Note that there are other, more advanced documentation tools available for Chicken, namely [hahn](https://wiki.call-cc.org/eggref/4/hahn), and [schematic](https://wiki.call-cc.org/eggref/5/schematic). Users of other Scheme implementations might also consider [SchemeDoc](http://people.cs.aau.dk/~normark/schemedoc/).


### Features

scm2wiki uses stand-alone comment blocks and commented definitions in Scheme source code to generate documentation in Markdown or svnwiki format. scm2wiki will only consider comments with a specific, user-defined prefix. By default, the prefix is ";;;". scm2wiki supports Markdown in comments, and optionally transforms Markdown to svnwiki syntax.

scm2wiki can generate documentation for the following definition types:

- variable and procedure definitions using `define`
- constant definitions using `define-constant`
- macro definitions using `define-syntax` (limited support)
- record type definitions using `define-record`, `define-record-type`, and `defstruct`
- coops class definitions using `define-class`
- coops generic procedure specializations using `define-method`

scm2wiki understands Chicken Scheme's native `module` declarations. By default, it will only generate documentation for symbols that are exported.

There is also some limited support for simple type annotations, including [typed records](https://wiki.call-cc.org/eggref/5/typed-records). However, this feature is not yet complete.


### Usage

`$ scm2wiki [options...]`

By default, scm2wiki reads from STDIN and outputs Markdown to STDOUT.
The following options are available:

option                 | function
-----------------------|-------
-i, --infile=FILENAME  | specify an input file
-o, --outfile=FILENAME | specify an output file
-p, --prefix=STRING    | change the comment prefix (default `";;;"`)
--svn                  | output to svnwiki instead of Markdown
--document-internals   | emit documentation for non-exported symbols


### Example

````scheme
;;; == A Level 1 Heading
;;; ## A Level 2 Heading
;;; A regular line
;;; A {{line}} with ''formatting''
;;; {{svnwiki}} syntax in comments is supported, as is **markdown** syntax

;;; A procedure definition
;;; The actual definition will be specially formatted and moved to the top of
;;; this comment block.
(define (my-fun arg)
  arg)

;; ignoring this one
(define (another-fun arg)
  '())

;;; A plain hyperlink: https://call-cc.org
;;; A [[#rel|relative link]] in svnwiki syntax
;;; A [named link](https://call-cc.org]] in markdown syntax

;; A code example in svnwiki syntax:
;;; <enscript highlight="scheme">
;;; (cons 'a '(b c))
;;; </enscript>

;; A code example in markdown syntax:
;;; ```scheme
;;; (cons 'd '(e f))
;;; ```

;; Inline code blocks are currently not supported.
````

becomes

```
== A Level 1 Heading
=== A Level 2 Heading
A regular line
A {{line}} with ''formatting''
{{svnwiki}} syntax in comments is supported, as is {{markdown}} syntax

<procedure>(my-fun arg)</procedure>
a procedure definition

A plain hyperlink: [[https://call-cc.org]]
A [[#rel|relative link]] in svnwiki syntax
A [[https://call-cc.org|named link]] in markdown syntax

<enscript highlight="scheme">
(cons 'a '(b c))
</enscript>

<enscript highlight="scheme">
(cons 'd '(e f))
</enscript>
```

respectively

````markdown
# A Level 1 Heading
## A Level 2 Heading
A regular line
A **line** with *formatting*
**svnwiki** syntax in comments is supported, as is **markdown** syntax

<pre>[PROCEDURE]
<b>(my-fun arg)</b></pre>
A procedure definition

A plain hyperlink: https://call-cc.org
A [relative link](#rel) in svnwiki syntax
A [named link](https://call-cc.org) in markdown syntax

```scheme
(cons 'a '(b c))
```

```scheme
(cons 'd '(e f))
```
````

Feel free to run scm2wiki on it's own source for a more detailed example.


### TODO

* [markdown] remove unsupported svnwiki tags
* [markdown] transform variadic procedure definitions, svnwiki style
* [markdown] automatic toc generation
* [all] better support for type annotations
* [all] better support for macro definitions
* ~~[all] more robust command line arguments parsing~~ *done*
* ~~[all] support for macro, record, constant definitions etc.~~ *done*


### Contributing

Contributions to scm2wiki are very welcome! See the above TODO list for issues that need help.
If you're unsure how to contribute or have any questions, feel free to open an
[issue](https://github.com/utz82/scm2wiki/issues).
