# Language and conventions #

---


There are several conventions and style guides for writing programs in OCaml. Here is a list of the most common to all of them we will follow.

  1. 80 column limit
  1. No tab characters
  1. Two space indents
  1. Code must compile (enforced by TDD)
  1. Comments go above the code they reference
  1. Avoid useless comments and over-commenting (_Don't hesitate to comment when there's a difficulty. If there's no difficulty, there's no point in commenting_)
  1. Use meaningful names for variables (exceptions are let blocks and temporary short-scoped variables)
  1. Type annotations for top-level variables and functions
  1. Avoid global mutable variables
  1. Naming: (see [2](http://www.seas.upenn.edu/~cis500/cis500-f06/resources/programming_style.html#14))
    * Variables, functions and types: lowecase, joined by underscores
    * Constructors, modules and functors: uppercase and camelcased
  1. Break long lines into smaller pieces
  1. Don't leave incomplete match expressions
  1. Don't misuse `if` expressions (use the shortcuts and structures provided by the language itself)

Since we are still learning OCaml language, some of the conventions that show discrepancy among the guides will be decided on the run, when we know more about the language and identify the pros and cons of them.

## References ##

  * [CS312 SML Style Guide](http://www.cs.cornell.edu/Courses/cs312/2001sp/style.html) (Cornell)
  * [Penn Engineering's guide](http://www.seas.upenn.edu/~cis500/cis500-f06/resources/programming_style.html)
  * [Caml programming guidelines](http://caml.inria.fr/resources/doc/guides/guidelines.en.html)
  * [CS134b OCaml Style Guide](http://mojave.caltech.edu/jyh/classes/cs134/cs134b/2005/public_html/style.html) (Caltech)