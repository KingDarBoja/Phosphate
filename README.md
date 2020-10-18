<p align="center">
  <img src="assets/logo.svg" width="256px"/>
</p>

# Phosphate

Phosphate is a Nim port of GraphQL.js, the JavaScript reference implementation of GraphQL created by Facebook.

## This is a work in progress!!

This port is based on the [graphql-core](https://github.com/graphql-python/graphql-core) python port which is also based on [graphql-js](https://github.com/graphql/graphql-js).

### TODO LIST

- **Language**
    - [x] Token Kind Module
    - [x] Directive Locations Module
    - [x] AST module
    - [ ] AST module tests
    - [x] Block String module
    - [x] Block String module tests
    - [x] Source module
    - [x] Source module tests
    - [x] Location module
    - [x] Location module tests
    - [x] Lexer module
    - [x] Lexer module tests
    - [x] Parser module
    - [x] Parser module tests
    - [ ] Predicates module
    - [ ] Predicates module tests
    - [x] Print Location module
    - [x] Print Location module tests
    - [ ] Printer module
    - [ ] Printer module tests
    - [ ] Visitor module
    - [ ] Visitor module tests
- **Errors**
    - [x] GraphQLError module
    - [x] GraphQLError module tests
    - [ ] LocatedError module
    - [ ] LocatedError module tests
    - [x] GraphQLSyntaxError module

The `location` module got merged into source module due to cyclic imports not being supported. Also, the `location` tests are available at the Python port, the JS implementation lack of it, same goes with `ast` tests.

## Development

### Setup Nim

Install Nim by using any available installer. Currently using [choosenim](https://github.com/dom96/choosenim) as it is easier to update and manage several versions on Windows 10.

### Run tests

Run `nimble test` and that's it!
