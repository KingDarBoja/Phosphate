import unittest, strutils, strformat

import language/ast
import language/parser
import language/visitor

suite "Visitor Test Suite":

  # setup:
  #   proc checkVisitorProcArgs(ast: GraphNode, p: VisitFuncParams) =


  test "Validates Path Argument":
    echo "----------------- Short AST -----------------"
    echo "Input: { a }"
    let ast = parse("{ a }", noLocation = true)

    var visited: seq[seq[string]] = @[]

    proc enter(p: VisitFuncParams): (VisitorCommand, GraphNode) =
      # checkVisitorProcArgs(ast, p)
      visited.add @["enter"] & p.path.join(", ")
      # echo "--------- ENTER CALL ---------"
      # echo "Path: " & p.path.join(", ") & fmt" Key: {p.key} - Kind: {p.node.kind}"
      result = (vcIdle, p.node)

    proc leave(p: VisitFuncParams): (VisitorCommand, GraphNode) =
      # checkVisitorProcArgs(ast, p)
      visited.add @["leave"] & p.path.join(", ")
      # echo "--------- ENTER CALL ---------"
      # echo "Path: " & p.path.join(", ") & fmt" Key: {p.key} - Kind: {p.node.kind}"
      result = (vcIdle, p.node)

    discard visit(ast, newVisitorOptions(enter = enter, leave = leave))
    echo "********** Visited Result **********"
    for i, v in visited.pairs:
      echo fmt"{i} - {v}"
    discard visitTest(ast)

  # test "Random Test":
  #   echo "----------------- Longer AST -----------------"
  #   echo "Input: { a, b, c { a, b, c } }"
  #   let ast = parse("{ a, b, c { a, b, c } }", noLocation = true)

  #   var visited: seq[string] = @[]

  #   proc enter(p: VisitFuncParams): (VisitorCommand, GraphNode) =
  #     visited.add "enter"
  #     visited.add $p.node.kind
  #     echo "--------- ENTER CALL ---------"
  #     echo p.path
  #     result = (vcIdle, p.node)
    
  #   discard visit(ast, newVisitorOptions(enter = enter))
  #   echo "--------- Visited Result ----------"
  #   echo visited