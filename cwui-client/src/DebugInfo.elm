module DebugInfo exposing (viewRemoteState)
import Html as H exposing (Html)
import Dict

import RemoteState exposing (RemoteState, Valuespace, TypeAssignMap, TypeMap, NodeMap)
import Cmp.Dict as CDict

viewTyAssns : TypeAssignMap -> Html a
viewTyAssns = H.table [] << Dict.foldl
    (\p (ts, ed) acc -> H.tr []
      [ H.td [] [H.text p]
      , H.td [] [H.text <| toString ts]
      , H.td [] [H.text <| toString ed]
      ] :: acc)
    []

viewTypeMap : TypeMap t -> Html a
viewTypeMap = H.table [] << CDict.foldl
    (\ts ty acc -> H.tr []
      [ H.td [] [H.text <| toString ts]
      , H.td [] [H.text <| toString ty]
      ] :: acc)
    []

viewNodes : NodeMap -> Html a
viewNodes = H.text << toString << Dict.keys

viewValuespace : Valuespace -> Html a
viewValuespace vs = H.div []
  [ H.h3 [] [H.text "Type Assignments"]
  , viewTyAssns <| .tyAssns vs
  , H.h3 [] [H.text "Tree Types"]
  , viewTypeMap <| .types vs
  , H.h3 [] [H.text "Post Types"]
  , viewTypeMap <| .postTypes vs
  , H.h3 [] [H.text "Nodes"]
  , viewNodes <| .nodes vs
  ]

viewRemoteState : RemoteState -> Html a
viewRemoteState rs = H.div [] <| CDict.foldl
    (\ns vs acc -> H.div [] [H.h2 [] [H.text <| toString ns], viewValuespace vs] :: acc)
    [] rs
