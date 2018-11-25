# Coral Language

Repository for the Coral language, a imperative pure functional language that's
run in the Python VM.

I'm developing this as a fun side project, so the things will be changing
slowly. I'm also using this to learn about the formalisms of a pure functional
language, type systems and compilers, so expect **very** wrong things at times :+1:.

The objective is to implement a language with the following features:

- A modern type system, with typeclasses, parametric polymorphism and all that stuff;
- Linear Types;
- A nice effect system;
- A custom RTS concurrent with the Python VM RTS.

The language is somewhat a mix of Python, Rust and Haskell into a unique
language. A exemple of what the language looks like can be seem in the
`test.cor` file.

## Development

```sh
# Build the project.
stack build

# Run the test suite.
stack test

# Run the benchmarks.
stack bench

# Generate documentation.
stack haddock
```
