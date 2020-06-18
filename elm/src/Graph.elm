module Graph exposing (Node, Edge)


type alias NodeProperties =
  { label: String
  , style: String
  }

defaultNodeProperties : NodeProperties
defaultNodeProperties =
  { label = ""
  , style = ""
  }

type Node = Node String NodeProperties

type alias EdgeProperties = 
  { label: String
  , style: String -- might want list of strings
  , arrowhead: String
  }

defaultEdgeProperties : EdgeProperties
defaultEdgeProperties =
  { label = ""
  , style = ""
  , arrowhead = ""
  }

type Edge = Edge Node Node EdgeProperties

edgeToDebugString : Edge -> String
edgeToDebugString e =
  case e of
    Edge (Node fromId _) (Node toId _) _ -> fromId ++ " -> " ++ toId

edgeToDot : Edge -> String
edgeToDot e =
  case e of
    Edge (Node fromId _) (Node toId _) _ -> fromId ++ " -> " ++ toId 


a : Node
a = Node "A" defaultNodeProperties

b : Node
b = Node "B"
  { defaultNodeProperties
  | label = "this is B!"
  }

edge : Edge
edge = Edge a b
  { defaultEdgeProperties
  | label = "test"
  }

-- easy to paste into repl version
{-
type alias Node = { id: String }
type alias Edge = { from: Node, to: Node }
edgeToString e = e.from.id ++ " -> " ++ e.to.id
-}
