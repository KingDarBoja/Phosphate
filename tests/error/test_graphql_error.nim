import unittest, options, strtabs

import language/ast
import language/parser
import language/source_location
import error/graphql_error

import nimutils/dedent

suite "Describe graphql error":

  setup:
    let src = newSource(
      dedent(
      """
      {
        field
      }
      """
      )
    )
    let parsedAST = parse(src)
    let operationNode = parsedAST.definitions[0]
    let operationDefNode = OperationDefinitionNode(operationNode)
    let castedOperationNode = Node(operationDefNode)
    let fieldNode = Node(operationDefNode.selectionSet.selections[0])

  test "Is a class and is a subclass of exception":
    check(newGraphQLError("str") of Exception)
    check(newGraphQLError("str") of GraphQLError)

  test "Has a name message and stack trace":
    let e = newGraphQLError("msg")
    check($(type(e)) == "GraphQLError")
    check(e.msg == "msg")

  test "Uses the stack of an original error":
    var original: ref Exception
    try:
      raise newException(Exception, "original")
    except Exception as err:
      original = err
    let e = newGraphQLError("msg", originalError = some(original))
    check($(type(e)) == "GraphQLError")
    check(e.msg == "msg")
    check(e.originalError == original)
    check(e.originalError.msg == "original")

  test "Converts nodes to positions and locations":
    let e = newGraphQLError("msg", some(@[fieldNode]))
    check(e.nodes == @[fieldNode])
    check(e.source == src)
    check(e.positions == @[4])
    check(e.locations == @[newSourceLocation(2, 3)])

  test "Converts single node to positions and locations":
    let e = newGraphQLError("msg", some(fieldNode))
    check(e.nodes == @[fieldNode])
    check(e.source == src)
    check(e.positions == @[4])
    check(e.locations == @[newSourceLocation(2, 3)])

  test "Converts node with loc start zero to positions and locations":
    let e = newGraphQLError("msg", some(castedOperationNode))
    check(e.nodes == @[castedOperationNode])
    check(e.source == src)
    check(e.positions == @[0])
    check(e.locations == @[newSourceLocation(1, 1)])

  test "Converts source and positions to locations":
    let e = newGraphQLError("msg", none(Node), some(src), some(@[6]))
    check(e.nodes.len == 0)
    check(e.source == src)
    check(e.positions == @[6])
    check(e.locations == @[newSourceLocation(2, 5)])

  test "Serializes to include message":
    let e = newGraphQLError("msg")
    check(e.msg == "msg")
    check(repr(e) == "GraphQLError(msg)")

  test "Serializes to include message and locations":
    let e = newGraphQLError("msg", some(fieldNode))
    check(repr(e) == "GraphQLError(msg, locations=[SourceLocation(line=2, column=3)])")

  test "Repr includes extensions":
    let e = newGraphQLError("msg", extensions={"foo": "bar"}.newStringTable)
    check(repr(e) == "GraphQLError(msg, extensions={foo: bar})")

  test "Serializes to include path":
    let samplePath = @["path", "3", "to", "field"]
    let e = newGraphQLError("msg", path=samplePath)
    check(e.path == samplePath)

suite "Describe print error":

  test "Prints an error without location":
    let error = newGraphQLError("Error without location")
    check(printError(error) == "Error without location")

  test "Prints an error using node without location":
    let error = newGraphQLError(
      "Error attached to node without location",
      some(Node(parse("{ foo }", noLocation = true)))
    )
    check(printError(error) == "Error attached to node without location")

  test "Prints an error with nodes from different sources":
    let
      firstDoc = parse(
        newSource(
          dedent(
            """
            type Foo {
              field: String
            }
            """
          ),
          "SourceA"
        )
      )
      firstOp = ObjectTypeDefinitionNode(firstDoc.definitions[0])

    check(not firstOp.isNil and firstOp is ObjectTypeDefinitionNode and firstOp.fields.len > 0)

    let
      firstField = firstOp.fields[0]
      secondDoc = parse(
        newSource(
          dedent(
            """
            type Foo {
              field: Int
            }
            """
          ),
          "SourceB"
        )
      )
      secondOp = ObjectTypeDefinitionNode(secondDoc.definitions[0])
    
    check(not firstOp.isNil and firstOp is ObjectTypeDefinitionNode and secondOp.fields.len > 0)

    let
      secondField = secondOp.fields[0]
      error = newGraphQLError(
        "Example error with two nodes",
        some(@[Node(firstField.`type`), Node(secondField.`type`)])
      )
      printedError = printError(error)
    
    check(printedError & "\n" == dedent(
      """
      Example error with two nodes

      SourceA:2:10
      1 | type Foo {
      2 |   field: String
        |          ^
      3 | }

      SourceB:2:10
      1 | type Foo {
      2 |   field: Int
        |          ^
      3 | }
      """
    ))
    check($error == printedError)
