# Package

version       = "0.0.1"
author        = "KingDarBoja"
description   = "Phosphate is a Nim port of GraphQL.js, the JavaScript reference implementation of GraphQL created by Facebook."
license       = "MIT"
srcDir        = "src"



# Dependencies

requires "nim >= 1.0.6"

task test, "Runs the test suite":
  exec "nim c -r tests/language/test_source"
  exec "nim c -r tests/language/test_token_kind"
