# scm2wiki

scm2wiki is a simple documentation tool for [Chicken Scheme](https://call-cc.org/). It generates svnwiki content from comments in a Scheme source file.

Note that there are other, more advanced documentation tools available for Chicken, namely [hahn](https://wiki.call-cc.org/eggref/4/hahn), and [schematic](https://wiki.call-cc.org/eggref/4/schematic). Users of other Scheme implementations might also consider [SchemeDoc](http://people.cs.aau.dk/~normark/schemedoc/).

### Features

Not many, to be honest. Any line starting with a special comment prefix (user definable, using ";;;" by default) is considered as input to the converter. Furthermore, procedure definitions that are directly preceded by a comment block are converted to `<procedure>` statements. These statements are moved to the top of their respective comment block.

### Usage

`$ scm2wiki infile.scm <outfile.wiki <prefix>>`

### Example

```scheme
;;; == My Test
;;; Another line

;;; a procedure definition
(define (my-fun arg)
  arg)

;; ignoring this one
(define (another-fun arg)
  '())
```

becomes

```
== My Test
Another Line

<procedure>(my-fun arg)</procedure>
a procedure definition
```

Feel free to run scm2wiki on it's own source for a more detailed example.

### Limitations

For now, scm2wiki only handles procedure definitions, and ignores everything else. It also cannot transform variadic function definitions.