import unittest
import strformat

import language/ast
import language/parser
import language/source_location
import language/token_kind
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
      arguments = selections[0].arguments
    check(arguments.len == 1)
    let value = arguments[0].value
    check(value.value == "Has a \\u0A0A multi-byte character.")

  # TODO: Provide fixtures
  # test "Parses kitchen sink":
  #   discard

  test "Allows non keywords anywhere a name is allowed":
    let nonKeywords = @[
      "on",
      "fragment",
      "query",
      "mutation",
      "subscription",
      "true",
      "false"
    ]
    for keyword in nonKeywords:
      # You can't define or reference a fragment named `on`.
      let fragmentName = if keyword == "on": "a" else: keyword
      let document = &"""
        query {keyword} {{
          ... {fragmentName}
          ... on {keyword} {{ field }}
        }}
        fragment {fragmentName} on Type {{
          {keyword}({keyword}: ${keyword})
            @{keyword}({keyword}: {keyword})
        }}
      """
      discard parse(document)

  test "Parses anonymous mutation operations":
    discard parse(
      """
      mutation {
        mutationField
      }
      """
    )

  test "Parses anonymous subscription operations":
    discard parse(
      """
      subscription {
        subscriptionField
      }
      """
    )

  test "Parses named mutation operations":
    discard parse(
      """
      mutation Foo {
        mutationField
      }
      """
    )

  test "Parses named subscription operations":
    discard parse(
      """
      subscription Foo {
        subscriptionField
      }
      """
    )

  # This test shows the disadvantages of using classic OOP 
  # to deal with AST stuff. Pretty much forced to use cast.
  test "Creates AST":
    let doc = parse(
      dedent(
        """
        {
          node(id: 4) {
            id,
            name
          }
        }
        """
      )
    )
    check(doc.loc == (0, 41))
    let definitions = doc.definitions
    check(definitions.len == 1)
    # definitions[0]
    let definition = cast[OperationDefinitionNode](definitions[0])
    check(definition.loc == (0, 40))
    check(definition.operation == OperationTypeNode.QUERY)
    check(definition.name.isNil)
    check(definition.variableDefinitions.len == 0)
    check(definition.directives.len == 0)
    # definitions[0] -> selectionSet
    var selectionSet = definition.selectionSet
    check(selectionSet.loc == (0, 40))
    # definitions[0] -> selectionSet -> selections
    var selections = selectionSet.selections
    check(selections.len == 1)
    var field = selections[0]
    check(field.loc == (4, 38))
    check(field.alias.isNil)
    var name = field.name
    check(name.loc == (4, 8))
    check(name.value == "node")
    # definitions[0] -> selectionSet -> selections[0] -> arguments
    let arguments = field.arguments
    check(arguments.len == 1)
    let argument = arguments[0]
    name = argument.name
    check(name.loc == (9, 11))
    check(name.value == "id")
    let value = argument.value
    check(value.loc == (13, 14))
    check(value.strValue == "4")
    check(argument.loc == (9, 14))
    check(field.directives.len == 0)
    # definitions[0] -> selectionSet -> selections[0] -> selectionSet
    selectionSet = field.selectionSet
    check(selectionSet.loc == (16, 38))
    selections = selectionSet.selections
    check(selections.len == 2)
    # definitions[0] -> selectionSet -> selections[0] -> selectionSet -> selections[0]
    field = selections[0]
    check(field.loc == (22, 24))
    check(field.alias.isNil)
    name = field.name
    check(name.loc == (22, 24))
    check(name.value == "id")
    check(field.arguments.len == 0)
    check(field.directives.len == 0)
    check(field.selectionSet.isNil)
    # definitions[0] -> selectionSet -> selections[0] -> selectionSet -> selections[1]
    field = selections[1]
    check(field.loc == (30, 34))
    check(field.alias.isNil)
    name = field.name
    check(name.loc == (30, 34))
    check(name.value == "name")
    check(field.arguments.len == 0)
    check(field.directives.len == 0)
    check(field.selectionSet.isNil)

  test "Creates AST from nameless query without variables":
    let doc = parse(
      dedent(
        """
        query {
          node {
            id
          }
        }
        """
      )
    )
    check(doc.loc == (0, 30))
    let definitions = doc.definitions
    check(definitions.len == 1)
    # definitions[0]
    let definition = cast[OperationDefinitionNode](definitions[0])
    check(definition.loc == (0, 29))
    check(definition.operation == OperationTypeNode.QUERY)
    check(definition.name.isNil)
    check(definition.variableDefinitions.len == 0)
    check(definition.directives.len == 0)
    # definitions[0] -> selectionSet
    var selectionSet = definition.selectionSet
    check(selectionSet.loc == (6, 29))
    # definitions[0] -> selectionSet -> selections
    var selections = selectionSet.selections
    check(selections.len == 1)
    var field = selections[0]
    check(field.loc == (10, 27))
    check(field.alias.isNil)
    var name = field.name
    check(name.loc == (10, 14))
    check(name.value == "node")
    check(field.arguments.len == 0)
    check(field.directives.len == 0)
    # definitions[0] -> selectionSet -> selections[0] -> selectionSet
    selectionSet = field.selectionSet
    check(selectionSet.loc == (15, 27))
    # definitions[0] -> selectionSet -> selections[0] -> selectionSet -> selections
    selections = selectionSet.selections
    check(selections.len == 1)
    field = selections[0]
    check(field.loc == (21, 23))
    check(field.alias.isNil)
    name = field.name
    check(name.loc == (21, 23))
    check(name.value == "id")
    check(field.arguments.len == 0)
    check(field.directives.len == 0)
    check(field.selectionSet.isNil)

  test "Allows parsing without source location information":
    let result = parse("{ id }", noLocation = true)
    check(result.loc.isNil)

  test "Experimental allows parsing fragment defined variables":
    let document = "fragment a($v: Boolean = false) on t { f(v: $v) }"
    discard parse(document, experimentalFragmentVariables = true)
    expect GraphQLSyntaxError:
      discard parse(document)

  test "Contains location information that only stringifies start end":
    let res = parse("{ id }")
    check($res.loc == "{ start: 0, end: 6 }")
    check(desc(res.loc) == "<Location 0:6>")

  test "Contains references to source":
    let
      source = newSource("{ id }")
      res = parse(source)
    check(not res.loc.isNil and res.loc.source == source)

  test "Contains references to start and end tokens":
    let
      res = parse("{ id }")
    check(not res.loc.isNil and res.loc.startToken.kind == TokenKind.SOF)
    check(not res.loc.isNil and res.loc.endToken.kind == TokenKind.EOF)

  test "Allows comments everywhere in the source":
    let res = parse(
      """# top comment
      {
        field # field comment
      }
      # bottom comment"""
    )
    if not res.loc.isNil:
      let topComment = res.loc.startToken.next
      check(not topComment.isNil and topComment.kind == TokenKind.COMMENT)
      check(not topComment.isNil and topComment.value == " top comment")
      let fieldComment = topComment.next.next.next
      check(not fieldComment.isNil and fieldComment.kind == TokenKind.COMMENT)
      check(not fieldComment.isNil and fieldComment.value == " field comment")
      let bottomComment = fieldComment.next.next
      check(not bottomComment.isNil and bottomComment.kind == TokenKind.COMMENT)
      check(not bottomComment.isNil and bottomComment.value == " bottom comment")


suite "Describe Parse Value":

  test "Parses null value":
    let res = parseValue("null")
    check(res.loc == (0, 4))

  test "Parses empty strings":
    let res = parseValue("\"\"")
    check(res.value == "")
    check(res.loc == (0, 2))
  
  test "Parses list values":
    let res = parseValue("[123 \"abc\"]")
    check(res.loc == (0, 11))
    let values = res.values
    check(values.len == 2)
    let valueOne = values[0]
    check(valueOne.loc == (1, 4))
    check(valueOne.strValue == "123")
    let valueTwo = values[1]
    check(valueTwo.loc == (5, 10))
    check(valueTwo.value == "abc")

  test "Parses block strings":
    let res = parseValue("[\"\"\"long\"\"\" \"short\"]")
    check(res.loc == (0, 20))
    let values = res.values
    check(values.len == 2)
    let valueOne = values[0]
    check(valueOne.loc == (1, 11))
    check(valueOne.value == "long")
    check(valueOne.`block`)
    let valueTwo = values[1]
    check(valueTwo.loc == (12, 19))
    check(valueTwo.value == "short")
    check(not valueTwo.`block`)


suite "Describe Parse Type":

  test "Parses well know types":
    let res = parseType("String")
    check(res.loc == (0, 6))
    let name = res.name
    check(name.loc == (0, 6))
    check(name.value == "String")

  test "Parses custom types":
    let res = parseType("MyType")
    check(res.loc == (0, 6))
    let name = res.name
    check(name.loc == (0, 6))
    check(name.value == "MyType")

  test "Parses list types":
    let res = parseType("[MyType]")
    check(res.loc == (0, 8))
    let stype = res.`type`
    check(stype.loc == (1, 7))
    let name = stype.name
    check(name.loc == (1, 7))
    check(name.value == "MyType")

  test "Parses non null types":
    let res = parseType("MyType!")
    check(res.loc == (0, 7))
    let stype = res.`type`
    check(stype.loc == (0, 6))
    let name = stype.name
    check(name.loc == (0, 6))
    check(name.value == "MyType")

  test "Parses nested types":
    let res = parseType("[MyType!]")
    check(res.loc == (0, 9))
    let stype = res.`type`
    check(stype.loc == (1, 8))
    let ntype = stype.`type`
    check(ntype.loc == (1, 7))
    let name = ntype.name
    check(name.loc == (1, 7))
    check(name.value == "MyType")
