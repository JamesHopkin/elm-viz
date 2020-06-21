module Graph exposing ( Graph(..), toDot,
  defaultCommonProperties,
  Node(..), NodeProperties, defaultNodeProperties,
  Edge(..), EdgeProperties, defaultEdgeProperties,
  makeEdge,
  makeGraph )

import Dict exposing ( Dict )

type Graph = Graph (List Node) (List Edge)

-- todo graph properties

type alias CommonProperties = 
  { label: String
  , style: String
  , fillcolor: String 
  }

defaultCommonProperties : CommonProperties
defaultCommonProperties =
  { label = ""
  , style = ""
  , fillcolor = ""
  }

commonPropertiesToStringList : CommonProperties -> List ( String, String )
commonPropertiesToStringList props =
  [ ( "label", props.label )
  , ( "style", props.style )
  , ( "fillcolor", props.fillcolor )
  ]


type alias NodeProperties =
  { common: CommonProperties
  }

defaultNodeProperties : NodeProperties
defaultNodeProperties =
  { common = defaultCommonProperties
  }

nodePropertiesToStringList : NodeProperties -> List ( String, String )
nodePropertiesToStringList props =
  commonPropertiesToStringList props.common ++
  [
  ]

type Node = Node String NodeProperties

type alias EdgeProperties = 
  { common: CommonProperties
  , arrowhead: String
  }

defaultEdgeProperties : EdgeProperties
defaultEdgeProperties =
  { common = defaultCommonProperties
  , arrowhead = ""
  }

edgePropertiesToStringList : EdgeProperties -> List ( String, String )
edgePropertiesToStringList props =
  commonPropertiesToStringList props.common ++
  [ ( "arrowhead", props.arrowhead )
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
                  |> propertiesToDot
              in
                id ++ suffix ++ ";"

        renderEdge : Edge -> String
        renderEdge edge = 
          case edge of
            Edge (Node fromId _) (Node toId _) _ -> fromId ++ " -> " ++ toId 

        lines = 
          "digraph G {" ::
            List.map renderNode nodes ++
            List.map renderEdge edges ++
            ["}"]
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
