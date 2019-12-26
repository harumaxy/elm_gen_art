module Lib.Branch exposing (..)

import AltMath.Alternative.ADT.Matrix4 exposing (..)
import AltMath.Alternative.ADT.Vector2 exposing (..)
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
        , shapes [ stroke Color.green, LineConf.lineWidth 3 ] (branch (w / 2) h 200 (degrees 30))
        ]


background w h =
    shapes [ fill (Color.rgb255 51 51 51), stroke Color.black ]
        [ rect ( 0, 0 ) w h ]


branch : Float -> Float -> Float -> Float -> List Shape
branch x y len theta =
    let
        toX =
            -len * sin theta

        toY =
            -len * cos theta
    in
    if len > 20 then
        path ( x, y ) [ lineTo ( x + toX, y + toY ) ]
            :: branch (x + toX) (y + toY) (len * 0.66) (theta + degrees 10)
            ++ branch (x + toX) (y + toY) (len * 0.88) (theta + degrees -20)

    else
        []



-- tree : Float -> Float -> Float -> Float -> List Shape
-- tree x y len theta =
--     []
