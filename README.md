# PLLispy - experimental

This package defines a [grammar](https://github.com/syallop/PLGrammar) for an
experimental [Programming Language](https://github.com/syallop/PL).

This grammar is [reversible](https://github.com/syallop/Reverisble) meaning it
can be executed both 'forward' and 'backward'. This means it can be interpreted
both as a [parser](https://github.com/syallop/PLParser) that transforms text
into the [PL](https://github.com/syallop/PL) AST and as a
[pretty printer](https://github.com/syallop/PLPrinter) which formats PL AST
into text.

Parsing and printing through this grammar has a roundtrip property
in that `parse . print . parse === parse` and `print . parse . print == print`.
This does not guarantee that the input remains unchanged, only that printing
does not parse back to a different AST or vice versa. In practise, this is used
to normalise whitespace, parenthesis and line breaks.

The Grammar defined in this package is 'lispy' in that nested expressions/ types
etc are grouped by surrounding parenthesis.

## Example Syntax
Below are examples of the Grammars this package defines for parsing/ printing PL
code. Most examples show the grammar as defined by
[PLGrammar](https://github.com/syallop/PLGrammar) and some example strings that
could be parsed.

Both code fragments and grammars are likely to become out of date. The test
cases under `/tests/*Spec.hs` can be run with `stack test` and validate Grammars
defined under `PLLispy/*`.

### Expressions
- The following syntax may appear where an expression is expected.
- Each expression can always contain extra surrounding parenthesis.
- Expressions can always contain extra preceeding (and following) space without altering their parse.
- When printing, single or no spaces are prefered. Printing currently leans towards inserting parenthesis.
- In some cases preceeding spaces or parenthesis are required.

Simplified Grammar:
```haskell
alternatives
  [ lambdaExpr
  , bigLamdaExpr
  , appExpr
  , bigAppExpr
  , sumExpr
  , productExpr
  , unionExpr
  , bindingExpr
  , caseAnalysis
  , betweenParens expr
  ]
```

#### Lambdas
Lambdas abstract over expressions.

Simplified Grammar:
```haskell
-- A token lambda character followed by an abstraction then an expression
"λ" */ (lamIso \$/ type \*/ spaceRequired */ expression)

lamIso :: Iso (Type,Expr) Expr
```

Example parses:

| Parses           | Description |
| ---------------- | ----------- |
| `λFoo (0)`       | A function accepting an expression of type `Foo` and returns that expression. Types must only be annotated on the abstracted variable. Variables are referenced by an index  f how many abstractions away they appear            |
| `λFoo (λBar 1)`  | Parentheses are used to nest expressions. This is a function accepting an expression of type `Foo` that returns a function that accepts an expression of type `Bar` and returns the first `Foo`-typed expression.      |

#### Binding
Refer to expressions bound by a lambda expression.

Simplified Grammar:
```haskell
longestMatching1 isDigit
```

Example parses:

| Parses           | Description |
| ---------------- | ----------- |
| `0`              | The nearest expression bound by a Lambda abstraction. I.E. Inside `λFoo (0)` would refer to the expression typed `Foo`. |
| `1`              | The expression not the 0th, but the first binding away. I.E. Inside `λFoo (λBar (1))` would refer to the expression typed `Foo`. |

#### Function application
Apply regular Lambdas to expressions.

Simplified Grammar:
```haskell
-- A token 'at' character followed by two expressions.
"@" */ (appIso \$/ exprI \*/ spaceRequired */ exprI)

appIso :: Iso (Expr,Expr) Expr
```

Example parses:

| Parses            | Description |
| ----------------- | ----------- |
| `@ 0 1`            | Apply the expression bound 0 lambdas away to the expression bound one lambda away |
| `@ (λFoo (0)) (*)` | Apply an identity function to the zero-product |

#### Sum expressions
An sum expression has an index within some larger some type, in which it is one alternative.

Simplified Grammar:
```haskell
-- A token '+' character followed by an index into the overall sum type,
-- the expression itself, then zero or many of the constituent sum types.
"+" */ (sumIso \$/ token natural \*/ (spaceRequired */ expression) \*/ (spaceRequired */ type))

sumIso :: Iso (Int, (Expr, [Type])) Expr
```

Example parses:

| Parses            | Description |
| ----------------- | ----------- |
| `+0 (*) (*) Nat)`  | A value of a sum type. The index within the greater sum is `0`. The value is `(*)` - the empty product type. The first type in the sum is `(*)` - the type of empty products. The second type in the sum is `Nat`. |

#### Product expressions
A product expression combines many expressions into one where the order is fixed.

Simplified Grammar:
```haskell
-- A token 'star' followed by zero or many expressions.
"*" */ productIso \*/ rmany (spaceRequired */ expression)

productIso :: Iso [Expr] Expr
```

Example parses:

| Parses            | Description |
| ----------------- | ----------- |
| `* 0 1 2`         | A product of expressions bound 0, 1 and 2 abstractions away. |
| `(*)`             | The empty product |
| `* (*) (*)`       | A product of two empty products. |

#### Union expressions
A union expression has a type index into some larger type, in which it is one alternative. This differs from a sum type which has an order and where values are indexed by their position.

Simplified Grammar:
```haskell
-- A token 'union', a type index into the overall union, the individual expression,
-- then zero or many types the union is part of.
"∪" */ (unionIso \$/ type
                 \*/ spaceRequired */ expression
                 \*/ (setIso \$/ rmany (spaceRequired */ type)))

unionIso :: Iso (Type, (Expr, Set Type)) Expr
setIso   :: Ord a => Iso [a] (Set a)
```

Example parses:

| Parses                | Description |
| --------------------- | ----------- |
| `∪ Foo 0 Foo Bar Baz` | A value of a Union type. The index within the greater union is the type `Foo`.  The value is `0` - a binding to an abstraction. The types that form the union are `Foo` `Bar` and `Baz`. |

#### Big Lambdas
Big Lambdas abstract over types, but still produce expressions.

Simplified Grammar:
```haskell
-- A token big lambda followed by a kind and and expression.
"Λ" */ (bigLamdaIso \$/ kind \*/ (spaceRequired */ expression))

bigLamdaIso :: Iso (Kind, Expr) Expr
```

Example parses:

| Parses               | Description |
| -------------------- | ----------- |
| `ΛKIND λ(?0) 0`      | A function accepting a _type_ of kind `KIND` that produces a function that accepts an _expression_ with the previously bound type to produce that expression |

#### Type binding
Refer to _types_ bound by a big lambda expression.

Simplified Grammar:
```haskell
"?" */ natural
```

Example parses:

| Parses               | Description |
| -------------------- | ----------- |
| `?0`                 | The nearest type bound by a Big Lambda abstraction. I.E. Inside `ΛKIND (?0)` would refer to the type kinded `KIND`. |
| `?1`                 | The type not the 0th, but the first binding away. I.E. Inside `ΛKINDA (ΛKINDB (?1))` would refer to the type kinded `KINDA`. |

#### Big application
Apply Big Lambdas - which abstract over types - to types to produce an expression.

Simplified Grammar:
```haskell
-- A token 'big at' followed by an expression and a type.
"#" */ bigAppIso \$/ expression \*/ spaceRequired type

bigAppIso :: Iso (Expr, Type) Expr
```

Example parses:

| Parses                   | Description  |
| ------------------------ | ------------ |
| `# (Λ KIND λ(?0) 0) Foo` | Apply the type `Foo` to a Big Lambda which takes types of kind `KIND`. |

### Types
This syntax may appear anywhere a type is expected. The same rule for expressions apply to types regarding parenthesis and whitespace.

#### Named
A Named type is associated with some 'externally' defined type definition. This
is 'external' in that the (current) expression/ type language provides no syntax
for creating these associations. Type names are currently an upper case
  character followed by zero or more lower case characters.

Simplified Grammar:
```haskell
-- An upper case character followed by zero or more lower case characters.
namedIso \$/ upper \*/ longestMatching isLower

namedIso :: Iso (Char, Text) Type
```

Example parses:

| Parses               | Description |
| -------------------- | ----------- |
| `Foo`                | The type associated with the name `Foo` |

#### Arrows
An arrow is the type of regular Lambda functions and denotes the type the
function accepts and the type of the expression produced.

Simplified Grammar:
```haskell
-- An arrow followed by two types.
"→" */ arrowIso \$/ type \*/ (spaceRequired */ type)

arrowIso :: Iso (Type, Type) Type
```

Example parses:

| Parses               | Description |
| -------------------- | ----------- |
| `→ (*) Foo`          | The type of a Lambda which binds the empty product and produces an expression with the named type `Foo` |

#### Sum types
Denotes the type of sum expressions as a left-to-right ordering of the constituent expression types.

Simplified Grammar:
```haskell
-- A plus followed by zero or many types
"+" */ sumTIso \$/ rmany (spaceRequired */ type)

sumTIso :: Iso [Type] Type
```

Example parses:

| Parses               | Description |
| -------------------- | ----------- |
| `+ Foo Bar Baz`      | The type of sum expressions that may be individually typed either `Foo`, `Bar` or `Baz` |
| `+`                  | The empty sum type with no alternatives |

#### Product types
Denotes the type of product expressions as a left-to-right ordering

Simplified Grammar:
```haskell
-- star followed by zero or many types.
"*" */ productIso \*/ rmany (spaceRequired type)

productTIso :: Iso [Type] Type
```

Example parses:

| Parses               | Description |
| -------------------- | ----------- |
| `* Foo Bar Baz`      | The type of product expressions that are typed `Foo`, `Bar`, `Baz` in order |
| `*`                  | The empty product type with no sub-expressions |

#### Union types
The type of union expressions.

Simplified Grammar:
```haskell
-- A union followed by zero or more types
"∪" */ unionIso . setIso \$/ rmany (spaceRequired */ type)

unionIso :: Iso (Set Type) Type
setIso :: Ord a => Iso [a] (Set a)
```

Example parses:

| Parses               | Description |
| -------------------- | ----------- |
| `∪ Foo Bar Baz`      | The type of union expressions that may individually be either `Foo`, `Bar` or `Baz` and are indexed by type rather than order |
| `∪`                  | The empty union type with no alternatives |

#### Big arrow
The type of big lambdas.

Simplified Grammar:
```haskell
-- TODO

bigArrowIso :: Iso (Kind, Type) Type
```

Example parses:

| Parses               | Description |
| -------------------- | ----------- |

#### Type lambda
A type level lambda corresponds is a lambda that abstracts over types to
produces types.

Simplified Grammar:
```haskell
-- A big lambda followed by a kind and a type
"Λ" */ typeLambdaIso \$/ kind \*/ (spaceRequired */ type)

typeLambdaIso :: Iso (Kind, Type) Type
```

Example parses:

| Parses               | Description |
| -------------------- | ----------- |
| `Λ KIND Foo`         | A type function that abstracts over a type with kind `KIND` and produces the type `Foo`. |
| `Λ KIND ?0`          | A type function that abstracts over a type with kind `KIND` and produces that type. |

#### Type application
Apply a type lambda to a type to produce a type.

Simplified Grammar:
```haskell
-- A big 'at' followed by two types
"#" */ typeAppIso \$/ type \*/ (spaceRequired type)

typeAppIso :: Iso (Type, Type) Type
```

Example parses:

| Parses               | Description |
| -------------------- | ----------- |
| `# (Λ KIND ?0) Foo`  | Apply the type `Foo` (kinded KIND) to the type lambda which returns the bound `Foo` |

### Case analysis
Expressions (and _not_ types) can be pattern matched by case analysis.

An entire case statement starts with "CASE" followed by a scrutinee expression then the
case branches. Zero or many case branches can exist, followed by a default
branch. Branches have two components, the pattern they match (which may contain
pattern bindings) and an expression in which any bindings are accessible as if
bound by lambda abstraction.

Branches are denoted by a `|` character. Abstractions are denoted by `?`
character.

Example case statement:

```haskell
CASE expression          -- Case analysis on some expression
  (
    (|? (0))             -- The first branch matches anything and returns it.
    (|? (*))             -- The second branch will not be reached, but would
                         -- return an empty product.
  )
  (+)                    -- The default branch will not be reached but returns the empty sum
```

Sums, products and unions can be pattern matched upon. Lambdas and other forms
of expression can not. Wherever an expression is allowed within a pattern, so is
a `?` which acts to bind that expression.

Simplified match grammar:

```haskell
alternatives
  [ bind
  , matchBinding
  , matchSum
  , matchProduct
  , matchUnion
  , betweenParens match
  ]
```

#### Bind
A bind matches the entire expression and binds it to be referenced as if
abstracted by a lambda. If a bind appears at the top level, the entire
expression is bound. If it appears as a sub expression, that sub-expression is
bound.

Example parses:

| Parses               | Description |
| -------------------- | ----------- |
| `?`                  | Match any expression value. Sums, Products, etc |
| `+0 ?`               | If the sum expression is matched index 0, bind its expression |
| `* ? ?`              | Match both expressions in a product expression |

#### Sums
A sum pattern matches a sum expression. The index matches the sum expressions
alternatives left to right starting at 0.

Example parses:

| Parses               | Description |
| -------------------- | ----------- |
| `+0 ?`               | Matches when a sum expression is at index 0 within its sum type. |
| `+0 (+1 ?)`          | Matches when a sum expression is at index 0 within its sum type and that contained expression is at index 1 within its sum type. |

#### Products
A product pattern matches a product expression. All contained expressions must
match for the product to match as a whole.

| Parses               | Description |
| -------------------- | ----------- |
| `* ? ?`              | Matches any two expressions within a product type |
| `* (+0 ?) (+1 ?)`    | Matches only when both nested sum expression match |

#### Unions
A union pattern matches a union expression. A type is used as the index into the
union.

| Parses               | Description |
| -------------------- | ----------- |
| `∪ Foo ?`            | Matches when the expression has the `Foo` type within the union. |
