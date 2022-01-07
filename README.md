# parsel

Parsel is a zero-dependency Scala library for working with Python code. You probably shouldn't use it.

It has:
* AST data types. They try to align with [Python's own AST data structures](https://docs.python.org/3/library/ast.html).
* Visitor-ish base classes for defining AST transformations.
* A parser for Python 3 programs. It's hand-written and is probably loaded with bugs.
* Macro-based string interpolators for embedding Python 3 quasiquotes in your Scala code.

## Parser

Parsel's parser is hand-written, because I couldn't make a performant parser with fastparse while working from
the [Python official grammar](https://docs.python.org/3/reference/grammar.html). I later discovered that this is because
the Python 3 grammar is left-recursive in several places, so it needs some not-quite-PEG tricks (which Python's own
parser does, but arent't mentioned in the grammar specification) and so can be pretty frustrating to try and parse (this
is probably why I couldn't find a pre-existing Python 3 parser). By the time I figured this out, I had already
hand-written most of the parser, so I just went with it.

As a result of being hand-written, it's probably loaded with catastrophic bugs. But, at least it's pretty fast.

The parser is in the `parsel-parser` module. After adding a dependency on that, you can parse some python statements
from a `String` using `parsel.parse.Parser`:

```scala
import parsel.ast.Module
import parsel.parse.Parser
val program: Module = Parser.parse(
  """def hooray():
    |  return "woo!"
    |""".stripMargin
)
```

This will throw a `ParseError` if it doesn't parse successfully. Did I mention that parsel is not functional? It
mutates and throws exceptions all over the place. But, at least it's pretty fast.


## Quasiquotes

Parsel's quasiquotes allow you to embed python ASTs using a string interpolator, and the content of the string will be
checked (well, parsed, at least) at compile time, giving a compiler error if the Python code is invalid. This is similar
to the quasiquotes of Scala 2 macros. You can use this feature by depending on the `parsel-quotes` module.

There three flavors of quasiquote, which are accessed by importing `parsel.quote.syntax._`:

* `py` quasiquotes contain Python statements. Quoted expressions are spliced eagerly as expression trees (see `Quotable`
  below), so this type of quasiquote results in a `Module` (which is just a container for a bunch of statements).
* `pyq` quasiquotes also contain Python statements, but quoted expressions are suspended rather than being spliced
  eagerly. The idea here is that a `Quotable` instance could use side-effects when quoting (e.g. quote a large object
  or a `DataFrame` by writing it to a URI, and splice the necessary code for reading it in Python) and suspending it
  allows control over when those side effects happen. So this type of quasiquote results in a `QuotedTree`, which
  becomes a usable AST when its `doQuote()` method is called (triggering any side-effects).
* `pye` quasiquotes contain a single Python expression, so this type of quasiquote results in an `Expr`.

In each of these, you can splice values which have a `Quotable` instance, or which themselves represent parsel ASTs (or
`Seq`s of ASTs).

Here's what using them looks like:

```scala
import parsel.ast._
import parsel.quote.QuotedTree
import parsel.quote.syntax._

val myString = "hello quasiquotes!"
val mySuffix = " Yippee!"

// This will result in an Expr, where myString is spliced in as:
//     Constant(StringLiteral("hello quasiquotes!"))
val exampleExpr: Expr = pye"my_function($myString)"

// This will result in an AST, where mySuffix is spliced in as:
//     Constant(StringLiteral(" Yippee!"))
// in the same fashion, and exampleExpr is spliced in as an AST directly
val example1: Module = py"""
    def my_function(str):
      return str + $mySuffix
    
    print($exampleExpr)
  """

// This will result in a QuotedTree. When doQuote is called, the resulting
// AST will contain code to memoize the spliced value of myString by binding
// it to a fresh variable name, which is substituted in the quoted code.
// This probably seems kind of useless, but I needed it for my motivating use case.
val example2: QuotedTree = pyq"""
    $example1
    print(my_function(my_function($myString)))
  """

val example2Result: Module = example2.doQuote()
```

### `Quotable`

Splicing values is accomplished by the `Quotable` typeclass. Any expression of type `T` that's spliced into a
quasiquote must have an instance of `Quotable[T]`. This typeclass just has a method `doQuote`, which takes a value
of type `T` and returns an `Expr` which represents that value. Instances are already defined for obvious constants, but
I'm not sure how opinionated parsel ought to be about how things like collections and such ought to be quoted.

## License & Copyright
Copyright 2022 Jeremy Smith

Licensed under the Apache License, Version 2.0 (the "License");
you may not use these files except in compliance with the License.
You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License. 
