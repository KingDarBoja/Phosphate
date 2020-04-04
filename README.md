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
    - [ ] Block String module Tests
    - [x] Source module
    - [x] Source module tests
    - [x] Location module (Merged into Source module)
    - [ ] Location module tests (Merged into Source module tests)
    - [x] Lexer module (Missing minor tweaks with unicode and hex values)
    - [x] Lexer module tests
    - [ ] Parser module
    - [ ] Parser module tests
    - [ ] Predicates module
    - [ ] Predicates module tests
    - [ ] Visitor module
    - [ ] Visitor module tests
- **Errors**
    - [ ] (WIP) GraphQLError module (requires Parser to continue)
    - [ ] (WIP) GraphQLError module tests
    - [ ] LocatedError module
    - [ ] LocatedError module tests
    - [ ] GraphQLSyntaxError module
    - [ ] GraphQLSyntaxError module tests

## Development

### Setup Nim

Install Nim by using any available installer. Currently using [choosenim](https://github.com/dom96/choosenim) as it is easier to update and manage several versions on Windows 10.

### Run tests

Run `nimble test` and that's it!