import unittest
import strformat

import language/ast
import language/source_location
import language/parser
import error/syntax_error
import nimutils/dedent

suite "Describe Parser":

  setup:
    proc assertSyntaxError(text: string, msg: string, location: SourceLocation) =
      var err: GraphQLSyntaxError
      try:
        discard parse(text)
      except GraphQLSyntaxError as error:
        err = error
      check(err.msg == &"Syntax Error: {msg}")
      check(err.locations == @[location])

  test "Parse provides useful errors":
    var err: GraphQLSyntaxError
    try:
      discard parse("{")
    except GraphQLSyntaxError as error:
      err = error
    check(err.msg == fmt"Syntax Error: Expected Name, found <EOF>.")
    check(err.positions == @[1])
    check(err.locations == @[newSourceLocation(1, 2)])
    check($err & "\n" == dedent(
      """
      Syntax Error: Expected Name, found <EOF>.

      GraphQL request:1:2
      1 | {
        |  ^
      """
    ))
    assertSyntaxError(
      "\n      { ...MissingOn }\n      fragment MissingOn Type",
      "Expected 'on', found Name 'Type'.",
      newSourceLocation(3, 26),
    )
    assertSyntaxError(
      "{ field: {} }",
      "Expected Name, found '{'.",
      newSourceLocation(1, 10),
    )
    assertSyntaxError(
      "notAnOperation Foo { field }",
      "Unexpected Name 'notAnOperation'.",
      newSourceLocation(1, 1),
    )
    assertSyntaxError(
      "...",
      "Unexpected '...'.",
      newSourceLocation(1, 1)
    )
    assertSyntaxError(
      "{ \"\"",
      "Expected Name, found String ''.",
      newSourceLocation(1, 3)
    )

  test "Parse provides useful error when using source":
    var err: GraphQLSyntaxError
    try:
      discard parse(newSource("query", "MyQuery.graphql"))
    except GraphQLSyntaxError as error:
      err = error
    check($err & "\n" == dedent(
      """
      Syntax Error: Expected '{', found <EOF>.

      MyQuery.graphql:1:6
      1 | query
        |      ^
      """
    ))

  test "Parses variable inline values":
    discard parse("{ field(complex: { a: { b: [ $var ] } }) }")

  test "Parses constant default values":
    assertSyntaxError(
      "query Foo($x: Complex = { a: { b: [ $var ] } }) { field }",
      "Unexpected '$'.",
      newSourceLocation(1, 37)
    )

  test "Parses variable definition directives":
    discard parse("query Foo($x: Boolean = false @bar) { field }")

  test "Does not accept fragments named on":
    assertSyntaxError(
      "fragment on on on { on }",
      "Unexpected Name 'on'.",
      newSourceLocation(1, 10)
    )

  test "Does not accpet fragments spread of on":
    assertSyntaxError(
      "{ ...on }",
      "Expected Name, found '}'.",
      newSourceLocation(1, 9)
    )

  test "Parses multi byte characters":
    let
      doc = parse(
        """
        { field(arg: "Has a \u0A0A multi-byte character.") }
        """
      )
      definitions = doc.definitions
    check(definitions.len == 1)
    let 
      selectionSet = cast[OperationDefinitionNode](definitions[0]).selectionSet
      selections = selectionSet.selections
    check(selections.len == 1)
    let 
      arguments = cast[FieldNode](selections[0]).arguments
    check(arguments.len == 1)
    let value = cast[StringValueNode](arguments[0].value)
    check(value.value == "Has a \u0A0A multi-byte character.")