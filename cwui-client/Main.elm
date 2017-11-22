import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import WebSocket

main = Html.program {
    init = init, update = update, subscriptions = subscriptions, view = view}

-- Model

type alias ClType = String
type alias ClValue = String
type alias ClPath = String
type alias ClTime = Int

type alias ClSeries = Dict ClTime ClValue

type alias ClNode =
  { typeInfo : Maybe ClType
  , values : ClSeries
  , pending : ClSeries
  }

emptyNode : ClNode
emptyNode = ClNode Nothing Dict.empty Dict.empty

type alias NodeMap = Dict ClPath ClNode

type alias Model =
  { nodes : NodeMap
  , partialEntry : String
  }

init : (Model, Cmd Msg)
init = (Model (Dict.singleton "lemon" emptyNode) "somestring", Cmd.none)

-- Update

type InterfaceEvent = UpPartial String | IfAdd String | IfDrop String
type NetworkEvent = NeAdd String | NeDrop String

type Msg = InterfaceEvent InterfaceEvent | NetworkEvent (List NetworkEvent)

networkUpdate1 : NetworkEvent -> NodeMap -> NodeMap
networkUpdate1 ne nodes = case ne of
    (NeAdd s) -> Dict.insert s emptyNode nodes
    (NeDrop s) -> Dict.remove s nodes

networkUpdate : List NetworkEvent -> NodeMap -> (NodeMap, Cmd Msg)
networkUpdate nes nodes = (List.foldl networkUpdate1 nodes nes, Cmd.none)

interfaceUpdate : InterfaceEvent -> Model -> (Model, Cmd Msg)
interfaceUpdate ie model = case ie of
    (UpPartial s) -> ({model | partialEntry = s}, Cmd.none)
    (IfAdd s) -> (model, WebSocket.send "ws://echo.websocket.org" (String.cons 'a' s))
    (IfDrop s) -> (model, WebSocket.send "ws://echo.websocket.org" (String.cons 'd' s))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    (NetworkEvent ne) ->
      let
        (newNodes, cmd) = networkUpdate ne (.nodes model)
      in
        ({model | nodes = newNodes}, cmd)
    (InterfaceEvent ie) -> interfaceUpdate ie model

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model = Sub.map NetworkEvent (WebSocket.listen "ws://echo.websocket.org" eventFromNetwork)

eventFromNetwork : String -> List NetworkEvent
eventFromNetwork s = case String.uncons s of
    (Just (h, t)) -> case h of
        'a' -> [NeAdd t]
        'd' -> [NeDrop t]
        _ -> []
    Nothing -> []

-- View

view : Model -> Html Msg
view {nodes, partialEntry} = Html.map InterfaceEvent (div [] [subControl partialEntry, viewPaths nodes])

subControl : String -> Html InterfaceEvent
subControl path = div []
  [ input [type_ "text", placeholder "Path", onInput (UpPartial)] []
  , button [onClick (IfAdd path)] [text "cons"]
  ]

viewPaths : NodeMap -> Html InterfaceEvent
viewPaths nodes = div [] (
    List.map (\s -> button [onClick (IfDrop s)] [ text s ]) (Dict.keys nodes))
