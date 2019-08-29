# Kuifje

A prototype for a Quantitative Information Flow aware programming language.

## Generating documentation

Run `cabal haddock` to generate the API documentation in HTML format.

## Defining a program

The syntax of the language is defined in the `src/Syntax.hs` file. You can use the predefined constructor functions and the combinator `<>` to define programs. As can be seen in the examples, using the `Control.Lens` library can simplify the implementation.

## Running the analysis

The function `hysem` from the `Semantics` module can be used to calculate the hyper-distributions based on a program and the input distributions.

## Examples

The following examples are implemented in this repository:

- The Monty-Hall problem: `Monty.hs`
- Defence against side-channels: `SideChannel.hs`
- Password checker: `Password.hs`
