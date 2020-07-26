import unittest

import language/ast
import language/parser
import language/visitor

suite "Visitor Test Suite":

  test "Validates Path Argument":
    echo "----------------- Short AST -----------------"
    echo "Input: { a }"
    let ast = parse("{ a }", noLocation = true)

    var visited: seq[string] = @[]

    proc enter(p: VisitFuncParams): (VisitorCommand, GraphNode) =
      visited.add "enter"
      visited.add $p.node.kind
      echo "--------- ENTER CALL ---------"
      echo p.path
      result = (vcIdle, p.node)

    discard visit(ast, newVisitorOptions(enter = enter))
    echo "--------- Visited Result ----------"
    echo visited

  test "Random Test":
    echo "----------------- Longer AST -----------------"
    echo "Input: { a, b, c { a, b, c } }"
    let ast = parse("{ a, b, c { a, b, c } }", noLocation = true)

    var visited: seq[string] = @[]

    proc enter(p: VisitFuncParams): (VisitorCommand, GraphNode) =
      visited.add "enter"
      visited.add $p.node.kind
      echo "--------- ENTER CALL ---------"
      echo p.path
      result = (vcIdle, p.node)
    
    discard visit(ast, newVisitorOptions(enter = enter))
    echo "--------- Visited Result ----------"
    echo visited