# Package

version       = "0.0.1"
author        = "KingDarBoja"
description   = "Phosphate is a Nim port of GraphQL.js, the JavaScript reference implementation of GraphQL created by Facebook."
license       = "MIT"
srcDir        = "src"



# Dependencies

requires "nim >= 1.0.6"
requires "unicodeplus >= 0.6.0"

task test, "Runs the test suite":
  exec "nim c -r tests/language/test_source_location"
  exec "nim c -r tests/language/test_block_string"
  exec "nim c -r tests/language/test_lexer"
  exec "nim c -r tests/language/test_parser"
  exec "nim c -r tests/language/test_print_location"
  exec "nim c -r tests/error/test_graphql_error"
  exec "nim c -r tests/nimutils/test_dedent"
