import Dict exposing (Dict)

import Html exposing (Html)
import Html.Attributes as Hattr
import Html.Events as Hevt

import ClTypes exposing (Path)
import Form exposing (..)
import Layout exposing (..)

type UiMode
  = UmEdit
  | UmView

type alias Model =
  { viewMode : UiMode
  , globalErrs : List String
  , layout : Layout Path
  , layoutFs : FormStore LayoutPath Path
  , data : Dict Path String
  , dataFs : FormStore Path String
  }

type Evt
  = LayoutUiEvt (FormUiEvent LayoutPath Path)
  | DataUiEvt (FormUiEvent Path String)
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
    { viewMode = UmEdit
    , globalErrs = []
    , layout = initialLayout
    , layoutFs = formStoreEmpty
    , data = Dict.empty
    , dataFs = formStoreEmpty
    }

-- FIXME: Doesn't go anywhere
sendDataChange : Path -> String -> Cmd Evt
sendDataChange p d = Cmd.none

feHandler : Model -> FormEvent k v -> (k -> v -> (Model, Cmd Evt)) -> (Model, Cmd Evt)
feHandler m fe submitHandler = case fe of
    FeNoop -> (m, Cmd.none)
    FeError msg -> update (GlobalErrEvt msg) m
    FeSubmit k v -> submitHandler k v

update : Evt -> Model -> (Model, Cmd Evt)
update evt m = case evt of
    GlobalErrEvt msg -> ({m | globalErrs = msg :: .globalErrs m}, Cmd.none)
    SwapViewMode -> case .viewMode m of
        UmEdit -> ({m | viewMode = UmView}, Cmd.none)
        UmView -> ({m | viewMode = UmEdit}, Cmd.none)
    LayoutUiEvt fue ->
      let
        (newLayoutFs, fe) = formUiUpdate fue <| .layoutFs m
        newM = {m | layoutFs = newLayoutFs}
      in feHandler newM fe <| \lp p -> case setLeafBinding lp p <| .layout m of
        Err msg -> update (GlobalErrEvt msg) newM
        Ok newLayout -> ({newM | layout = newLayout, layoutFs = formClear lp <| .layoutFs m}, Cmd.none)
    DataUiEvt fue ->
      let
        (newDataFs, fe) = formUiUpdate fue <| .dataFs m
        newM = {m | dataFs = newDataFs}
      in feHandler newM fe <| \p d -> (newM, sendDataChange p d)

subscriptions : Model -> Sub Evt
subscriptions m = Sub.none

view : Model -> Html Evt
view m = Html.div []
  [ Html.text <| toString <| .globalErrs m
  , Html.button [Hevt.onClick SwapViewMode] [Html.text "switcheroo"]
  , case .viewMode m of
    UmEdit -> Html.map LayoutUiEvt <| layoutEditView (.layoutFs m) (.layout m)
    UmView -> Html.map DataUiEvt <| dataEditView (.dataFs m) (.data m) (.layout m)
  ]

containerHtml : List (Html a) -> Html a
containerHtml = Html.div []

layoutEditView : FormStore LayoutPath Path -> Layout Path -> Html (FormUiEvent LayoutPath Path)
layoutEditView fs =
  let
    go lp l = case l of
        LayoutContainer kids -> containerHtml <| List.indexedMap (\i -> go (lp ++ [i])) kids
        LayoutLeaf p -> case formState lp fs of
            FsViewing -> Html.span [Hevt.onClick <| FuePartial lp p] [Html.text p]
            FsEditing partial -> Html.span []
              [ Html.button [Hevt.onClick <| FueSubmit lp] [Html.text <| "Replace " ++ p]
              , Html.input [ Hattr.value partial, Hattr.type_ "text", Hevt.onInput <| FuePartial lp] []
              ]
            FsPending pending -> Html.span [Hevt.onClick <| FuePartial lp pending] [Html.text <| p ++ " -> " ++ pending]
  in go []

dataEditView : FormStore Path String -> Dict Path String -> Layout Path -> Html (FormUiEvent Path String)
dataEditView fs d l = case l of
    LayoutContainer kids -> containerHtml <| List.map (dataEditView fs d) kids
    LayoutLeaf p -> case Dict.get p d of
        Nothing -> Html.text "Awaiting data..."
        Just v -> case formState p fs of
            FsViewing -> Html.text "viewing: "
            FsEditing partial -> Html.text "editing: "
            FsPending pending -> Html.text "pending: "
