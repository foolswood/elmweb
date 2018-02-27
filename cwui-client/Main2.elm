import Dict exposing (Dict)

import Html exposing (Html)
import Html.Attributes as Hattr
import Html.Events as Hevt

import ClTypes exposing (Path)
import UiControl exposing (..)

type ControlPath
  = CpApi Path
  | CpLayout LayoutPath

type UiMode
  = UmEdit
  | UmView

type alias Model =
  { layout : Layout Path
  , formView : FormView ControlPath
  , viewMode : UiMode
  , globalErrs : List String
  , data : Dict Path FormState
  }

type Evt
  = FormWidgetEvt FormWidgetEvent
  | GlobalErrEvt String
  | SwapViewMode

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
      [ LayoutLeaf "/foo"
      , LayoutLeaf "/boo"
      ]
  in
    { layout = initialLayout
    , formView = layoutEdit initialLayout
    , viewMode = UmEdit
    , globalErrs = []
    , data = Dict.empty
    }

pEditForm : LayoutPath -> Path -> FormView LayoutPath
pEditForm lp p = FormWidget lp [EsText p]

layoutEdit : Layout Path -> FormView ControlPath
layoutEdit = mapFormView CpLayout << layoutEditorForm pEditForm

handleLayoutEdit : LayoutPath -> FormState -> Layout Path -> Result String (Layout Path)
handleLayoutEdit p fs = case fs of
    [EsText tgt] -> setLeafBinding p tgt
    _ -> always <| Err "Layout update pattern match failed"

update : Evt -> Model -> (Model, Cmd Evt)
update evt m = case evt of
    GlobalErrEvt msg -> ({m | globalErrs = msg :: .globalErrs m}, Cmd.none)
    SwapViewMode -> case .viewMode m of
        UmEdit -> ({m | viewMode = UmView, formView = dataEdit (.data m) <| .layout m}, Cmd.none)
        UmView -> ({m | viewMode = UmEdit, formView = layoutEdit <| .layout m}, Cmd.none)
    FormWidgetEvt e ->
      let
        (fv, fe) = widgetUpdate e <| .formView m
        newM = {m | formView = fv}
      in case fe of
        FormNoop -> (newM, Cmd.none)
        FormError msg -> update (GlobalErrEvt msg) newM
        FormUpdate idx fs -> case idx of
            CpLayout lp -> case handleLayoutEdit lp fs (.layout m) of
                Ok newLayout ->
                  ( {m
                    | layout = newLayout
                    , formView = layoutEdit newLayout}
                  , Cmd.none)
                Err msg -> update (GlobalErrEvt msg) newM
            CpApi p -> case handleDataEdit p fs (.data m) of
                Ok newData -> ({newM | data = newData, formView = dataEdit newData (.layout m)}, Cmd.none)
                Err msg -> update (GlobalErrEvt msg) newM

subscriptions : Model -> Sub Evt
subscriptions m = Sub.none

dataEdit : Dict Path FormState -> Layout Path -> FormView ControlPath
dataEdit d = mapFormView CpApi << dataEditorForm d

dataEditorForm : Dict Path FormState -> Layout Path -> FormView Path
dataEditorForm d l = case l of
    LayoutContainer kids -> FormContainer <| List.map (dataEditorForm d) kids
    LayoutLeaf p -> FormWidget p <| Maybe.withDefault [EsText "a", EsText "b"] <| Dict.get p d

handleDataEdit : Path -> FormState -> Dict Path FormState -> Result String (Dict Path FormState)
handleDataEdit p fs = Ok << Dict.insert p fs

view : Model -> Html Evt
view m = Html.div []
  [ Html.text <| toString <| .globalErrs m
  , Html.button [Hevt.onClick SwapViewMode] [Html.text "switcheroo"]
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
