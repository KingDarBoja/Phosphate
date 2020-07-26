import tables, sequtils, strformat, strutils

import language/ast

const QueryDocumentKeys: Table[string, seq[string]] = 
  {
    "Name": @[],
    "Document": @["definitions"],
    "OperationDefinition": @[
      "name",
      "variableDefinition",
      "directives",
      "SelectionSet"
    ],
    "VariableDefinition": @[
      "variable",
      "type",
      "defaultValue",
      "directives"
    ],
    "Variable": @["name"],
    "SelectionSet": @["selections"],
    "Field": @[
      "alias",
      "name",
      "arguments",
      "directives",
      "selectionSet"
    ],
    "Argument": @["name", "value"],
    "FragmentSpread": @["name", "directives"],
    "InlineFragment": @[
      "typeCondition",
      "directives",
      "selectionSet"
    ],
    "FragmentDefinition": @[
      "name",
      "variableDefinitions",
      "typeCondition",
      "directives",
      "selectionSet"
    ],
    "IntValue": @[],
    "FloatValue": @[],
    "StringValue": @[],
    "BooleanValue": @[],
    "NullValue": @[],
    "EnumValue": @[],
    "ListValue": @["values"],
    "ObjectValue": @["fields"],
    "ObjectField": @["name", "value"],
    "Directive": @["name", "arguments"],
    "NamedType": @["name"],
    "ListType": @["type"],
    "NonNullType": @["type"],
    "SchemaDefinition": @[
      "description",
      "directives",
      "operationTypes"
    ],
    "OperationTypeDefinition": @["type"],
    "ScalarTypeDefinition": @[
      "description",
      "name",
      "directives"
    ],
    "ObjectTypeDefinition": @[
      "description",
      "name",
      "interfaces",
      "directives",
      "fields"
    ],
    "FieldDefinition": @[
      "description",
      "name",
      "arguments",
      "type",
      "directives"
    ],
    "InputValueDefinition": @[
      "description",
      "name",
      "type",
      "defaultValue",
      "directives"
    ],
    "InterfaceTypeDefinition": @[
      "description",
      "name",
      "interfaces",
      "directives",
      "fields"
    ],
    "UnionTypeDefinition": @[
      "description",
      "name",
      "directives",
      "types"
    ],
    "EnumTypeDefinition": @[
      "description",
      "name",
      "directives",
      "values"
    ],
    "EnumValueDefinition": @[
      "description",
      "name",
      "directives"
    ],
    "InputObjectTypeDefinition": @[
      "description",
      "name",
      "directives",
      "fields"
    ],
    "DirectiveDefinition": @[
      "description",
      "name",
      "arguments",
      "locations"
    ],
    "SchemaExtension": @[
      "directives",
      "operationTypes"
    ],
    "ScalarTypeExtension": @[
      "name",
      "directives"
    ],
    "ObjectTypeExtension": @[
      "name",
      "interfaces",
      "directives",
      "fields"
    ],
    "InterfaceTypeExtension": @[
      "name",
      "interfaces",
      "directives",
      "fields"
    ],
    "UnionTypeExtension": @[
      "name",
      "directives",
      "types"
    ],
    "EnumTypeExtension": @[
      "name",
      "directives",
      "values"
    ],
    "InputObjectTypeExtension": @[
      "name",
      "directives",
      "fields"
    ],
  }.toTable


type
  VisitorCommand* = enum
    ##[
      Special return values for the visitor methods.
      Note that in GraphQL.js these are defined differently:
        - BREAK = {}        -> stop visiting
        - SKIP = false      -> skip visiting this node
        - REMOVE = null     -> delete this node
        - IDLE = undefined  -> no action
      
      Any value will replace the node with the returned value.
    ]## 
    vcBreak, vcSkip, vcRemove, vcIdle

  VisitFuncParams* = object
    ##[
      A visitor is comprised of visit functions, which are called on each node
      during the visitor's traversal.
    ]##
    node*: GraphNode
    ## The current node being visiting.
    key*: int
    ## The index or key to this node from the parent node or Array.
    parent*: GraphNode
    ## The parent immediately above this node, which may be an Array.
    path*: seq[int]
    ## The key path to get to this node from the root node.
    ancestors*: seq[GraphNode]
    ##[
      All nodes and Arrays visited before reaching parent of this node.
      These correspond to array indices in `path`.
      Note: ancestors includes arrays which contain the parent of visited node.
    ]##

  VisitFun* = proc (p: VisitFuncParams): tuple[cmd: VisitorCommand, rn: GraphNode]

  NamedVisitFuncs* = object
    kind*: VisitFun       # 1. Named visitors triggered when entering a node of a specific kind.
    enter*: VisitFun      # 2. Named visitors that trigger upon entering and leaving a node of a specific kind.
    leave*: VisitFun      # 2. Named visitors that trigger upon entering and leaving a node of a specific kind.

  VisitorOptions* = ref object
    kindFunMap*: Table[GraphNodeKind, NamedVisitFuncs]   # 1 and 2
    enter*: VisitFun       # 3. Generic visitors that trigger upon entering and leaving any node.
    leave*: VisitFun       # 3. Generic visitors that trigger upon entering and leaving any node.
    enterKindMap*: Table[GraphNodeKind, VisitFun]  # 4. Parallel visitors for entering and leaving nodes of a specific kind.
    leaveKindMap*: Table[GraphNodeKind, VisitFun]  # 4. Parallel visitors for entering and leaving nodes of a specific kind.

  VisitorEdit* = ref object
    key*: int
    value*: GraphNode

  VisitorStack* = ref object
    index*: int
    keys*: seq[string]
    edits*: seq[VisitorEdit]
    inSlice*: bool
    prev*: VisitorStack


proc initVisitFuncParams*(
  node: GraphNode = nil,
  key = 0,
  parent: GraphNode = nil,
  path: seq[int] = @[],
  ancestors: seq[GraphNode] = @[]
): VisitFuncParams =
  VisitFuncParams(node: node, key: key, parent: parent, path: path, ancestors: ancestors)


proc newVisitorOptions*(
  kindFunMap = initTable[GraphNodeKind, NamedVisitFuncs](),
  enter: VisitFun = nil,
  leave: VisitFun = nil,
  enterKindMap = initTable[GraphNodeKind, VisitFun](),
  leaveKindMap = initTable[GraphNodeKind, VisitFun](),
): VisitorOptions =
  new(result)
  result.kindFunMap = kindFunMap
  result.enterKindMap = enterKindMap
  result.leaveKindMap = leaveKindMap
  if not enter.isNil:
    result.enter = enter
  if not leave.isNil:
    result.leave = leave


proc getVisitFn*(
  visitorOpts: VisitorOptions,
  kind: GraphNodeKind,
  isLeaving: bool
): VisitFun =
  if visitorOpts.isNil:
    result = nil
  elif visitorOpts.kindFunMap.hasKey(kind):
    let kindVisitor = visitorOpts.kindFunMap[kind]
    if not isLeaving and not kindVisitor.kind.isNil:
      # { Kind() {} }
      result = kindVisitor.kind
    elif isLeaving:
      # { Kind: { leave() {} } }
      result = kindVisitor.leave
    else:
      # { Kind: { enter() {} } }
      result = kindVisitor.enter
  elif isLeaving:
    # Generic Visitor
    if not visitorOpts.leave.isNil:
      # { leave() {} }
      result = visitorOpts.leave
    if visitorOpts.leaveKindMap.hasKey(kind):
      # { leave: { Kind() {} } }
      result = visitorOpts.leaveKindMap[kind]
  else:
    # Generic Visitor
    if not visitorOpts.enter.isNil:
      # { enter() {} }
      echo "-- GENERIC: Check if has enter! --"
      result = visitorOpts.enter
    if visitorOpts.enterKindMap.hasKey(kind):
      # { enter: { Kind() {} } }
      result = visitorOpts.leaveKindMap[kind]
        

proc visit*(
  root: GraphNode,
  visitor: VisitorOptions,
  # visitorKeys: seq[GraphNodeKind] = @[]
): (VisitorCommand, GraphNode) =
  ##[
    Visit each node in an AST.

    visit() will walk through an AST using a depth first traversal, calling the
    visitor's enter methods at each node in the traversal, and calling the leave methods
    after visiting that node and all of its child nodes.

    By returning different values from the enter and leave methods, the behavior of the
    visitor can be altered, including skipping over a sub-tree of the AST (by returning
    false), editing the AST by returning a value or Nil to remove the value, or to stop
    the whole traversal by returning BREAK.

    When using visit() to edit an AST, the original AST will not be modified,
    and a new version of the AST with the changes applied will be returned from the
    visit function.
  ]##
  var
    stack: VisitorStack
    parent: GraphNode
    parentSlice: seq[GraphNode]
    inSlice = false
    prevInSlice = false
    keys: seq[int] = @[root.kind.ord]
    index = -1
    edits: seq[VisitorEdit]
    path: seq[int]
    ancestors: seq[GraphNode]
    ancestorsSlice: seq[seq[GraphNode]]
    newRoot = root

  while true:
    inc(index)
    echo fmt"Index: {index}"
    let
      isLeaving = index == keys.len
      isEdited = isLeaving and edits.len > 0
    var
      key: int
      node: GraphNode
      parent: GraphNode

    if isLeaving:
      key = if ancestors.len > 0: path[^1] else: -1
      node = parent
      parent = if ancestors.len > 0: ancestors.pop() else: nil
      echo fmt"Leaving: {root.kind}"
    else:
      if not parent.isNil:
        if inSlice:
          key = index
          node = parent.children[key]
        else:
          key = keys[index]
          node = parent.children[key]
      else:
        key = -1
        node = newRoot
      
      if node.isNil:
        continue
      if not parent.isNil:
        path.add(key)

    if not node.isNil:
      let visitProc = getVisitFn(visitor, node.kind, isLeaving)
      echo "-------- VISIT FUN --------"
      if not visitProc.isNil:
        echo "Reaching code"
        result = visitProc(initVisitFuncParams(node, key, parent, path, ancestors))

    # echo "------ RESULT --------"
    # echo path
    # echo repr(result)
    break;



# proc visitTest*(
#   root: GraphNode,
#   parent: GraphNode = nil,
#   level: int = 1
# ): GraphNode =
#   # Playing with echo to have some good looking AST printed for debugging
#   let newRoot: GraphNode = root
#   if level == 1:
#     echo "  ".repeat(level) & fmt" Parent Node: {newRoot.kind}"
#   case newRoot.kind
#   of gnkName, gnkIntValue, gnkFloatValue, gnkStringValue, gnkBooleanValue, gnkNullValue, gnkEmpty, gnkEnumValue, gnkOperationType:
#     # Apply visitor options to this Node
#     echo "    ".repeat(level) & fmt" Leaf Node: {newRoot.kind} "
#   of gnkSelectionSet:
#     if not parent.isNil:
#       # Parent in this case is gnkField
#       for idx, child in parent.children.mpairs:
#         if child.kind == gnkSelectionSet:
#           # You can't remove this children since we are still iterating over it
#           # parent.children.delete(idx)
#           # For now, setting to nil
#           parent.children[idx] = nil
#           break;
#   else:
#     # Recursively Walk and Apply visitor to every children
#     if newRoot.children.len > 0:
#       for idx, child in newRoot.children.pairs:
#         echo "    ".repeat(level).join("") & fmt" Child Node: {child.kind}"
#         discard visitTest(child, newRoot, level + 1)
#     else:
#       echo "    ".repeat(level).join("") & " No Children"

