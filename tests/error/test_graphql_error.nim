import unittest
import options
import typetraits

import error/graphql_error
# import language/source
import language/ast

# import nimutils/dedent

suite "Describe graphql error":

  # setup:
  #   let src = Source(
  #     dedent(
  #       """
  #     {
  #       field
  #     }
  #     """
  #     )
  #   )
  #   let parsedAST = parse()

  test "Is a class and is a subclass of exception":
    check(initGraphQLError("str") of Exception)
    check(initGraphQLError("str") of GraphQLError)

  test "Has a name message and stack trace":
    let e = initGraphQLError("msg")
    check(e.type.name == "GraphQLError")
    check(e.msg == "msg")

  test "Uses the stack of an original error":
    var original: ref Exception
    try:
      raise newException(Exception, "original")
    except Exception as err:
      original = err
    let e = initGraphQLError("msg", originalError = some(original))
    check(e.type.name == "GraphQLError")
    check(e.msg == "msg")
    check(e.originalError == original)
    check(e.originalError.msg == "original")

  # test "Converts nodes to positions and locations":
  #   let e = GraphQLError(msg: "msg")