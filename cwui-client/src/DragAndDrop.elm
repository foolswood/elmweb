module DragAndDrop exposing
  ( EffectAllowed(..), DropEffect(..), DataTransfer, MayHandle
  , onDragStart, dragStartAttr
  , onDragEnter
  , DragOverEvent, dragOverAttr
  , onDrop
  )

import Dict exposing (Dict)
import Json.Encode as JE
import Json.Decode as JD
import Html as H
import Html.Attributes as HA
import Html.Events as HE

import Native.DragAndDrop

type alias MimeType = String

type EffectAllowed
  = EaNone
  | EaCopy
  | EaCopyLink
  | EaCopyMove
  | EaLink
  | EaLinkMove
  | EaMove
  | EaAll
  | EaUninitialized

effectAllowedStr : EffectAllowed -> String
effectAllowedStr ea = case ea of
    EaNone -> "none"
    EaCopy -> "copy"
    EaCopyLink -> "copyLink"
    EaCopyMove -> "copyMove"
    EaLink -> "link"
    EaLinkMove -> "linkMove"
    EaMove -> "move"
    EaAll -> "all"
    EaUninitialized -> "uninitialized"

effectAllowedFromStr : String -> EffectAllowed
effectAllowedFromStr s =
    if s == "none" then EaNone
    else EaUninitialized

type DropEffect
  = DropCopy
  | DropMove
  | DropLink
  | DropNone

type alias DataTransfer = Dict MimeType String

type alias DragEndEvent = {effect : DropEffect, dataTransfer : DataTransfer}
type alias DragOverEvent = {effectAllowed : EffectAllowed, dataTransfer : DataTransfer}
type alias DropEvent = {effect : DropEffect, dataTransfer : DataTransfer}

type alias MayHandle = {preventDefault : Bool, stopPropagation : Bool}

-- NOTE: At present these aren't cleaned and are thus only for global handlers
newRef : a -> String
newRef = Native.DragAndDrop.newRef

type DomEvent = DomEvent

applyMayHandle : DomEvent -> MayHandle -> ()
applyMayHandle domEvt {preventDefault, stopPropagation} = throwoutReturns
  [ if preventDefault then setPreventDefault domEvt else ()
  , if stopPropagation then setStopPropagation domEvt else ()
  ]

setStopPropagation : DomEvent -> ()
setStopPropagation = Native.DragAndDrop.stopPropagation

setPreventDefault : DomEvent -> ()
setPreventDefault = Native.DragAndDrop.preventDefault

setDataTransfer : DomEvent -> Dict MimeType JE.Value -> ()
setDataTransfer evt = Native.DragAndDrop.setDataTransfer evt << JE.encode 0 << JE.object << Dict.toList 

setEffectAllowed : DomEvent -> EffectAllowed -> ()
setEffectAllowed evt effectAllowed = Native.DragAndDrop.setEffectAllowed evt <| effectAllowedStr effectAllowed

handlerAttr : String -> a -> H.Attribute msg
handlerAttr evtType handler = HA.attribute
    ("on" ++ evtType)
    ("Foolswood.callRef(" ++ newRef handler ++ ", event)")

-- Drag start

dragStartAttr : EffectAllowed -> Dict MimeType JE.Value -> H.Attribute evt
dragStartAttr effectAllowed dataTransfer = handlerAttr "dragstart" <| handleDragStart (effectAllowed, dataTransfer)

handleDragStart : (EffectAllowed, Dict MimeType JE.Value) -> DomEvent -> ()
handleDragStart (effectAllowed, dataTransfer) evt = throwoutReturns
  [ setEffectAllowed evt effectAllowed
  , setDataTransfer evt dataTransfer
  ]

onDragStart : evt -> H.Attribute evt
onDragStart = HE.on "dragstart" << JD.succeed

-- Drag enter

onDragEnter : evt -> H.Attribute evt
onDragEnter = HE.on "dragenter" << JD.succeed

-- Drag over

dragOverAttr : (DragOverEvent -> MayHandle) -> H.Attribute msg
dragOverAttr = handlerAttr "dragover" << handleDragOver

dragOverEvent : DomEvent -> DragOverEvent
dragOverEvent evt =
  let
    effectAllowed = Native.DragAndDrop.getEffectAllowed evt
    dataTransfer = Native.DragAndDrop.getDataTransfer evt
  in
    { effectAllowed = effectAllowedFromStr effectAllowed
    , dataTransfer = Maybe.withDefault Dict.empty <| Result.toMaybe <| JD.decodeString (JD.dict JD.string) dataTransfer
    }

handleDragOver : (DragOverEvent -> MayHandle) -> DomEvent -> ()
handleDragOver handler evt = applyMayHandle evt <| handler <| dragOverEvent evt

-- Drop

onDrop : evt -> H.Attribute evt
onDrop = HE.onWithOptions "drop" {preventDefault = True, stopPropagation = False} << JD.succeed

-- Utilitat

throwoutReturns : List () -> ()
throwoutReturns _ = ()
