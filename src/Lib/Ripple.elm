module Lib.Ripple exposing (..)

import Browser.Events exposing (..)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Color
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Events.Extra.Mouse as Mouse
import List exposing (..)



-- Model


type Model
    = List Ripple


type alias Ripple =
    { pos : ( Float, Float ), r : Float, maxr : Float }



-- Update


type Msg
    = ClickedAt ( Float, Float )


update : List Ripple -> Float -> List Ripple
update model dt =
    let
        speed =
            100
    in
    model
        |> List.map (\item -> { item | r = item.r + speed * dt / 1000 })
        |> List.filter (\item -> item.r < item.maxr)



-- View


view model =
    let
        w =
            1000

        h =
            1000
    in
    Canvas.toHtml ( w, h )
        [ Mouse.onDown (\event -> ClickedAt event.offsetPos) ]
        ([ background w h
         , shapes [ fill Color.red ] [ circle ( 0, 0 ) 100 ]
         ]
            ++ List.map renderRipples model
        )


background w h =
    shapes [ fill (Color.rgb255 51 51 51), stroke Color.black ]
        [ rect ( 0, 0 ) w h ]


renderRipples : Ripple -> Renderable
renderRipples item =
    shapes [ fill (Color.rgba 1 1 1 (1 - item.r / item.maxr)) ] [ circle item.pos item.r ]
