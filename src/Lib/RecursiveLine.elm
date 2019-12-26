module Lib.RecursiveLine exposing (..)

import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Line as LineConf exposing (..)
import Color exposing (..)



-- View


view =
    let
        w =
            1000

        h =
            1000
    in
    Canvas.toHtml ( w, h )
        []
        [ background w h
        , shapes [ stroke Color.red, LineConf.lineWidth 5 ] (recursiveLine (w / 2) (h / 2) 500)
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
