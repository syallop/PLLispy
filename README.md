# PLLispy - experimental

This package defines a [grammar](https://github.com/syallop/PLGrammar) for an experimental [Programming Language](https://github.com/syallop/PL). This grammar is [reversible](https://github.com/syallop/Reverisble) meaning it can be executed both 'forward' and 'backward'. This means it can be interpreted both as a [parser](https://github.com/syallop/PLParser) that transforms text into the [PL](https://github.com/syallop/PL) AST and as a [pretty printer](https://github.com/syallop/PLPrinter) which formats PL AST into text. Parsing and printing through this grammar has a roundtrip property in that `parse . print . parse === parse` and `print . parse . print == print`. This does not guarantee that the input remains unchanged, only that printing does not parse back to a different AST or vice versa.

## Example Syntax
The Grammar defined in this package is 'lispy' in that nested expressions/ types
etc are grouped by surrounding parenthesis. The syntax is defined against a
specific version of PL. Therefore the following snippets are rough examples that
are likely to be inexhaustive or become out of date.
The test cases under `/tests/*Spec.hs` can be ran with `stack test` and are more
likely to remain up to date.

### Lambdas

| Name             | Parses           | Description |
| -----------------| ---------------- | ----------- |
| Lambdas          |                  |             |
| Simple           | `λFoo (0)`       | A function accepting an expression of type `Foo` and returns that expression. Types must only be annotated on the abstracted variable. Variables are referenced by an index  f how many abstractions away they appear            |
| Nested           | `λFoo (λBar 1)`  | Parentheses are used to nest expressions. This is a function accepting an expression of type `Foo` that returns a function that accepts an expression of type `Bar` and returns the first `Foo`-typed expression.      |
| Chained          | `λFoo Bar Baz 2` | Interpreted as `λFoo (λBar (λ Baz 2))`.      |
| Big Lambdas (Abstract types)        |             |
| Simple           | `ΛKIND λ(?0) 0`  |             |

