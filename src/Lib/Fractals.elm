module Lib.Fractals exposing (..)

import Canvas exposing (..)
import Canvas.Settings exposing (..)
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
        , shapes [ stroke Color.white ] [ circle ( w / 2, h / 2 ) 300 ]
        , shapes [ stroke Color.red ] (recursiveCircle (w / 2) (h / 2) 300)
        ]


background w h =
    shapes [ fill (Color.rgb255 51 51 51), stroke Color.black ]
        [ rect ( 0, 0 ) w h ]


recursiveCircle : Float -> Float -> Float -> List Shape
recursiveCircle x y r =
    if r > 10 then
        circle ( x, y ) r
            :: recursiveCircle (x + r) y (r / 2)
            ++ recursiveCircle (x - r) y (r / 2)
        -- ++ recursiveCircle (x - r) (y + r) (r / 2)

    else
        [ circle ( x, y ) 10 ]


recursiveLine : Float -> Float -> Float -> List Shape
recursiveLine x y len =
    if len > 1 then
        path ( x, y ) [ lineTo ( x + len, y ) ] :: recursiveLine (x - len / 2) (y - 50) (len / 2)

    else
        [ path ( x, y ) [ lineTo ( x + len, y ) ] ]
