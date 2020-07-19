import tables, sequtils

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
  VisitorCommand = enum
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

  VisitFuncParams = object
    ##[
      A visitor is comprised of visit functions, which are called on each node
      during the visitor's traversal.
    ]##
    node: GraphNode
    ## The current node being visiting.
    key: int
    ## The index or key to this node from the parent node or Array.
    parent: GraphNode
    ## The parent immediately above this node, which may be an Array.
    path: seq[string]
    ## The key path to get to this node from the root node.
    ancestors: seq[GraphNode]
    ##[
      All nodes and Arrays visited before reaching parent of this node.
      These correspond to array indices in `path`.
      Note: ancestors includes arrays which contain the parent of visited node.
    ]##

  VisitFun = proc (p: VisitFuncParams): (VisitorCommand, GraphNode)

  NamedVisitFuncs = object
    kind*: VisitFun       # 1. Named visitors triggered when entering a node of a specific kind.
    enter*: VisitFun      # 2. Named visitors that trigger upon entering and leaving a node of a specific kind.
    leave*: VisitFun      # 2. Named visitors that trigger upon entering and leaving a node of a specific kind.

  VisitorOptions = ref object
    kindFunMap*: Table[GraphNodeKind, NamedVisitFuncs]   # 1 and 2
    enter*: VisitFun       # 3. Generic visitors that trigger upon entering and leaving any node.
    leave*: VisitFun       # 3. Generic visitors that trigger upon entering and leaving any node.
    enterKindMap*: Table[GraphNodeKind, VisitFun]  # 4. Parallel visitors for entering and leaving nodes of a specific kind.
    leaveKindMap*: Table[GraphNodeKind, VisitFun]  # 4. Parallel visitors for entering and leaving nodes of a specific kind.

  VisitorEdit = ref object
    key: int
    value: GraphNode

  VisitorStack = ref object
    index: int
    keys: seq[string]
    edits: seq[VisitorEdit]
    inSlice: bool
    prev: VisitorStack


proc popNodeSlice(a: seq[seq[GraphNode]]): (seq[GraphNode], seq[seq[GraphNode]]) =
  if a.len == 0:
    return (@[], @[])
  return (a[^1], a[0..^1])


proc removeNodeByIndex(a: seq[GraphNode], pos: int): seq[GraphNode] =
  if pos >= a.len:
    return a
  return concat(a[0 ..< pos], a[pos + 1 ..^ 1])


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
      result = visitorOpts.enter
    if visitorOpts.enterKindMap.hasKey(kind):
      # { enter: { Kind() {} } }
      result = visitorOpts.leaveKindMap[kind]
  result = nil
        

proc visit*(
  root: GraphNode,
  visitor: VisitorOptions,
  visitorKeys: GraphNodeKind
): GraphNode =
  ##[
    Visit each node in an AST.

    visit() will walk through an AST using a depth first traversal, calling the
    visitor's enter methods at each node in the traversal, and calling the leave methods
    after visiting that node and all of its child nodes.

    By returning different values from the enter and leave methods, the behavior of the
    visitor can be altered, including skipping over a sub-tree of the AST (by returning
    false), editing the AST by returning a value or Nil to remove the value, or to stop
    the whole traversal by returning BREAK.

    When usingvisit() to edit an AST, the original AST will not be modified,
    and a new version of the AST with the changes applied will be returned from the
    visit function.
  ]##
  var
    newRoot: GraphNode = root
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

  while true:
    inc(index)
    let
      isLeaving = index == keys.len
      isEdited = isLeaving and edits.len > 0
    var
      key: int
      node: GraphNode = nil
      nodeSlice: seq[GraphNode]
    # key: string for structs or int for slices.
    # node: GraphNode or can be anything
    if isLeaving:
      var
        key, path = path.pop
        node = parent
        parent, ancestors = ancestors.pop
        nodeSlice = parentSlice
        parentSlice, ancestorsSlice = popNodeSlice(ancestorsSlice)
      if isEdited:
        let prevInSlice = inSlice
        var editOffset = 0
        for editIndex, edit in edits.mpairs:
          if inSlice:
            if edit.value.isNil:
              nodeSlice = removeNodeByIndex(nodeSlice, edit.key - editOffset)
              inc(editOffset)
            else:
              nodeSlice[edit.key - editOffset] = edit.value
          else:
            # Not sure about this one
            nodeSlice[edit.key - editOffset] = edit.value
      index = stack.index
      keys = stack.keys
      edits = stack.edits
      inSlice = stack.inSlice
      stack = stack.prev
    else:
      # Get Key
      if not parent.isNil:
        if inSlice:
          key = index
        else:
          key = keys[index]
        node = parentSlice[key]

