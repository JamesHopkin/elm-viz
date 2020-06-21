module Graph exposing ( Graph(..), toDot,
  Node(..), NodeProperties, defaultNodeProperties,
  Edge(..), EdgeProperties, defaultEdgeProperties,
  makeEdge,
  makeGraph )

import Dict exposing ( Dict )

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

nodePropertiesToStringList : NodeProperties -> List ( String, String )
nodePropertiesToStringList props =
  [ ( "label", props.label )
  , ( "style", props.style )
  ]

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

edgePropertiesToStringList : EdgeProperties -> List ( String, String )
edgePropertiesToStringList props =
  [ ( "label", props.label )
  , ( "style", props.style )
  ]

type Edge = Edge Node Node EdgeProperties

edgeToDebugString : Edge -> String
edgeToDebugString e =
  case e of
    Edge (Node fromId _) (Node toId _) _ -> fromId ++ " -> " ++ toId

edgeToDot : Edge -> String
edgeToDot e =
  case e of
    Edge (Node fromId _) (Node toId _) _ -> fromId ++ " -> " ++ toId 

propertiesToDot : List ( String, String ) -> String
propertiesToDot lst =
  let
      newLst = List.filter (\(_,y) -> y /= "") lst
      contents = newLst  
        |> List.map (\(x,y) -> "\"" ++ x ++ "\" = \"" ++ y ++ "\"") 
        |> String.join " , "        
  in
      if List.isEmpty newLst then
          ""
      else 
          "[" ++ contents ++ "]"

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
                suffix = props
                |> nodePropertiesToStringList
                |> propertiestoDot
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

makeEdge : Dict String Node -> String -> String -> EdgeProperties -> Maybe Edge
makeEdge nodeDict fromId toId props =
  let
    maybeFrom = Dict.get fromId nodeDict
    maybeTo = Dict.get toId nodeDict
  in
    case ( maybeFrom, maybeTo ) of
      ( Just from, Just to ) -> Just ( Edge from to props )
      _ -> Nothing

makeNodesDict : List ( String, NodeProperties ) -> Dict String Node
makeNodesDict nodes =
  let
    makeEntry ( id, props ) = ( id, Node id props )
  in
    Dict.fromList (List.map makeEntry nodes)


makeGraph : List Node -> List ( String, String, EdgeProperties ) -> Graph
makeGraph nodes edgeDefs =
  let
    nodeToIdAndNode : Node -> ( String, Node )
    nodeToIdAndNode node =
      case node of
        Node id _ -> ( id, node )

    nodesDict : Dict String Node
    nodesDict = Dict.fromList <| List.map nodeToIdAndNode nodes
    edgeFromDef : ( String, String, EdgeProperties ) -> Maybe Edge
    edgeFromDef ( fromId, toId, props ) = makeEdge nodesDict fromId toId props

    edges = List.map edgeFromDef edgeDefs

  in
    Graph nodes (List.filterMap identity edges)
