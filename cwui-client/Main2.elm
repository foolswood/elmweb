import Html exposing (Html)
import Html.Attributes as Hattr
import Html.Events as Hevt

import UiControl exposing (..)

type alias Model =
  { layout : Layout
  , formView : FormView LayoutPath
  , globalErrs : List String
  }

type Evt
  = FormWidgetEvt FormWidgetEvent
  | GlobalErrEvt String

main = Html.program
  { init = (initModel, Cmd.none)
  , update = update
  , subscriptions = subscriptions
  , view = view
  }

initModel : Model
initModel =
  let
    initialLayout = LayoutContainer
      [ LayoutLeaf <| CpApi "/foo"
      , LayoutLeaf <| CpApi "/boo"
      ]
  in
    { layout = initialLayout
    , formView = layoutEditorForm initialLayout
    , globalErrs = []
    }

update : Evt -> Model -> (Model, Cmd Evt)
update evt m = case evt of
    GlobalErrEvt msg -> ({m | globalErrs = msg :: .globalErrs m}, Cmd.none)
    FormWidgetEvt e ->
      let
        (fv, fe) = widgetUpdate e <| .formView m
        newM = {m | formView = fv}
      in case fe of
        FormNoop -> (newM, Cmd.none)
        FormError msg -> update (GlobalErrEvt msg) newM
        FormUpdate idx fs -> case handleLayoutEdit idx fs (.layout m) of
            Ok newLayout -> ({m | layout = newLayout, formView = layoutEditorForm newLayout}, Cmd.none)
            Err msg -> update (GlobalErrEvt msg) newM

subscriptions : Model -> Sub Evt
subscriptions m = Sub.none

view : Model -> Html Evt
view m = Html.div []
  [ Html.text <| toString <| .globalErrs m
  , Html.map FormWidgetEvt <| viewForm [] <| .formView m
  ]

replaceIdx : Int -> a -> List a -> Result String (List a)
replaceIdx idx v l = if List.length l > idx
  then Ok <| List.take idx l ++ v :: List.drop (idx + 1) l
  else Err "Index out of range"

viewForm : FormPath -> FormView a -> Html FormWidgetEvent
viewForm fp fv = case fv of
    FormWidget _ fs ->
      let
        wotsit i newS = case replaceIdx i newS fs of
            Ok newFs -> FwUpdate fp newFs
            Err msg -> FwError msg
        thingy i s = Html.map (wotsit i) <| viewEntryState s
      in
        Html.span [] <| Html.button [Hevt.onClick <| FwSubmit fp] [Html.text "submit"] :: List.indexedMap thingy fs
    FormContainer kids -> Html.div [] <| List.indexedMap (\i -> viewForm (fp ++ [i])) kids

viewEntryState : EntryState -> Html EntryState
viewEntryState es = case es of
    EsText s -> Html.input [Hattr.value s, Hattr.type_ "text", Hevt.onInput EsText] []
