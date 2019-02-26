# scm2wiki

scm2wiki is a simple documentation tool for Scheme, written in [Chicken Scheme](https://call-cc.org/). It generates documentation from comments in a Scheme source file. scm2wiki can output to Chicken's native svnwiki format, as well as (Github flavored) Markdown.

Note that there are other, more advanced documentation tools available for Chicken, namely [hahn](https://wiki.call-cc.org/eggref/4/hahn), and [schematic](https://wiki.call-cc.org/eggref/4/schematic). Users of other Scheme implementations might also consider [SchemeDoc](http://people.cs.aau.dk/~normark/schemedoc/).

### Features

Not many, to be honest. scm2wiki understands both svnwiki and Markdown syntax. Any line starting with a special comment prefix (user definable, using ";;;" by default) is considered as input to the converter. The example below documents all the syntax that is supported by scm2wiki.

### Usage

`$ scm2wiki -i infile.scm [-o outfile.wiki] [-p prefix] [-m]`

By default, scm2wiki outputs in svnwiki format. To output to Markdown, use the `-m` flag.

If the `-o` specifier is omitted, the output filename will be post-fixed with ".wiki" or ".md", depending on which output mode is invoked.

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

### Limitations

For now, scm2wiki only handles procedure definitions, and ignores everything else.

### TODO

* [markdown] remove unsupported svnwiki tags
* [markdown] transform variadic procedure definitions, svnwiki style
* [markdown] automatic toc generation
* [all] more robust command line arguments parsing
* [all] support for macro, record, constant definitions etc.
