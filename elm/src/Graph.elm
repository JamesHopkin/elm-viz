module Graph exposing ( Graph(..), toDot,
  Node(..), NodeProperties, defaultNodeProperties,
  Edge(..), EdgeProperties, defaultEdgeProperties )

type Graph = Graph (List Node) (List Edge)

-- todo graph properties

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

testedge : Edge
testedge = Edge a b
  { defaultEdgeProperties
  | label = "test"
  }

-- easy to paste into repl version
{-
type alias Node = { id: String }
type alias Edge = { from: Node, to: Node }
edgeToString e = e.from.id ++ " -> " ++ e.to.id
-}

toDot graph = 
  case graph of
    Graph nodes edges ->
      let
        renderNode : Node -> String
        renderNode node = 
          case node of
            Node id props ->
              let
                suffix = case props.label of 
                  "" -> ""
                  _ -> " [label=\"" ++ props.label ++ "\"]"
              in
                id ++ suffix ++ ";"

        renderEdge : Edge -> String
        renderEdge edge = 
          case edge of
            Edge (Node fromId _) (Node toId _) _ -> fromId ++ " -> " ++ toId 

        lines = 
          [ "digraph G {" ] ++
          (List.map renderNode nodes) ++
          (List.map renderEdge edges) ++
          [ "}" ]
      in
        String.join "\n" lines

  --"""digraph G {
  --  node [shape=box];
  --  From [href="javascript:linkTest('woo!')"];

  --  From -> Elm [style=dashed, arrowhead=none];
  --  From -> another;
  --}
  --"""