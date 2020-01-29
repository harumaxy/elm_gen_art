module Lib.RecursiveLine exposing (..)

import Browser.Dom exposing (..)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Line as LineConf exposing (..)
import Color exposing (..)
import Html exposing (..)



-- View


type alias Scene =
    { width : Float
    , height : Float
    }


view : Scene -> Html msg
view scene =
    let
        w =
            scene.width

        h =
            scene.height
    in
    Canvas.toHtml ( round w, round h )
        []
        [ background w h
        , shapes [ stroke Color.red, LineConf.lineWidth 5 ] (recursiveLine (w / 2) (h / 2) 400)
        ]


background w h =
    shapes [ fill (Color.rgb255 51 51 51), stroke Color.black ]
        [ rect ( 0, 0 ) w h ]


recursiveLine : Float -> Float -> Float -> List Shape
recursiveLine x y len =
    if len > 10 then
        path ( x - len * 1 / 3, y ) [ lineTo ( x + len / 3, y ) ]
            :: recursiveLine (x - len / 3) (y + 50) (len / 2)
            ++ recursiveLine (x + len / 3) (y + 50) (len / 2)

    else
        [ path ( x, y ) [ lineTo ( x + len, y ) ] ]
