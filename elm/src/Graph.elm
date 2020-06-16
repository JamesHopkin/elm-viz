module Graph exposing (Node, Edge)

type alias Node =
  { id: String
  }

type alias Edge =
  { from: Node
  , to: Node
  }

edgeToString : Edge -> String
edgeToString e = e.from.id ++ " -> " ++ e.to.id

a = { id = "A" }
b = { id = "B" }
edge = { from = a, to = b }

