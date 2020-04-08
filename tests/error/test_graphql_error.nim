import unittest
import options
import typetraits

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
    let operationDefNode = cast[OperationDefinitionNode](operationNode)
    let fieldNode = cast[Node](operationDefNode.selectionSet.selections[0])

  test "Is a class and is a subclass of exception":
    check(newGraphQLError("str") of Exception)
    check(newGraphQLError("str") of GraphQLError)

  test "Has a name message and stack trace":
    let e = newGraphQLError("msg")
    check(e.type.name == "GraphQLError")
    check(e.msg == "msg")

  test "Uses the stack of an original error":
    var original: ref Exception
    try:
      raise newException(Exception, "original")
    except Exception as err:
      original = err
    let e = newGraphQLError("msg", originalError = some(original))
    check(e.type.name == "GraphQLError")
    check(e.msg == "msg")
    check(e.originalError == original)
    check(e.originalError.msg == "original")

  test "Converts nodes to positions and locations":
    let e = newGraphQLError("msg", some(@[fieldNode]))
    check(e.nodes == @[fieldNode])
