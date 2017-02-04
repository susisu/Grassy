# Grassy
Toolkit for the [Grass](http://www.blue.sky.or.jp/grass/) programming language.

## Installation
``` shell
git clone https://github.com/susisu/Grassy.git
cd Grassy
stack install
```

## Usage
### `grass`
`grass` is a Grass interpreter (not so fast).

``` shell
grass <filename>
```

or

``` shell
grass -e <program>
```

See [the original website of Grass](http://www.blue.sky.or.jp/grass/) for detailed information about the language.

### `plant`
`plant` is a *transpiler* from untyped lambda calculus (written in ML-like syntax) to Grass.

``` shell
plant <input> -o <output>
```

`plant -h` shows help about the avaiable options.


The syntax of the source language is briefly described by the following BNF:

```
<prog> ::= <def>+
<def>  ::= "let" <pattern>+ "=" <term>
<term> ::= <ident>                                  -- variable
         | <term> <term>                            -- application
         | "fun" <pattern>+ "->" <term>             -- lambda abstraction
         | "let" <pattern>+ "=" <term> "in" <term>  -- local binding

<ident>   ::= <letter> (<letter> | <digit> | "_" | "'")*
<pattern> ::= <ident> | "_"
```

`_` pattern does not bind any variable.

`w`, `Out`, `In`, and `Succ` (the primitives of Grass) are exposed to the global environment.

The last value defined in the source will be called with itself as an argument (as the definition of the Grass language).

## Author
Susisu ([GitHub](https://github.com/susisu), [Twitter](https://twitter.com/susisu2413))

## References
- http://www.blue.sky.or.jp/grass/
