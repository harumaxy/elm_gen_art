module Lib.Branch exposing (..)

import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Line as LineConf exposing (..)
import Color exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Material.Slider exposing (..)
import Round exposing (..)



-- Model


type alias Model =
    { left_degrees : Float
    , right_degrees : Float
    , left_decay : Float
    , right_decay : Float
    }


init : Model
init =
    Model 30 30 0.66 0.66



-- Update


type Msg
    = RightDegrees Float
    | RightDecay Float
    | LeftDegrees Float
    | LeftDecay Float


update : Msg -> Model -> Model
update msg model =
    case msg of
        RightDegrees val ->
            { model | right_degrees = val }

        RightDecay val ->
            { model | right_decay = val }

        LeftDegrees val ->
            { model | left_degrees = val }

        LeftDecay val ->
            { model | left_decay = val }



-- View


view : Model -> Html Msg
view model =
    let
        w =
            1000

        h =
            1000
    in
    div []
        [ h2 [] [ Html.text "スライダーにフォーカスして ← / → キーで1目盛りずつ動かせます" ]
        , div [ style "width" "50%", style "margin" "auto", style "display" "flex", style "flex-wrap" "wrap" ]
            [ div [ id "LeftBranchController", style "display" "flex", style "flex-direction" "column", style "flex" "1" ]
                [ p [] [ Html.text "Left branch" ]
                , makeSlider ("角度: " ++ Round.round 2 model.left_degrees) 0 90 1 model.left_degrees (Just LeftDegrees)
                , makeSlider ("減衰係数: " ++ Round.round 2 model.left_decay) 0 0.7 0.01 model.left_decay (Just LeftDecay)
                ]
            , div [ id "RightBranchController", style "display" "flex", style "flex-direction" "column", style "flex" "1" ]
                [ h2 [] [ Html.text "Right branch" ]
                , makeSlider ("角度: " ++ Round.round 2 model.right_degrees) 0 90 1 model.right_degrees (Just RightDegrees)
                , makeSlider ("減衰係数: " ++ Round.round 2 model.right_decay) 0 0.7 0.01 model.right_decay (Just RightDecay)
                ]
            ]

        -- , makeSlider 0 10 1 5 Nothing
        , Canvas.toHtml ( w, h )
            []
            [ background w h
            , shapes [ stroke Color.green, LineConf.lineWidth 3 ] (branch (w / 2) h 300 (degrees 0) model)
            ]
        ]


makeSlider : String -> Float -> Float -> Float -> Float -> Maybe (Float -> Msg) -> Html Msg
makeSlider title min max step initVal onChange =
    div [ style "flex" "1", style "margin" "10px", style "min-width" "200px" ]
        [ h2 [ style "margin" "0px" ] [ Html.text title ]
        , slider
            { sliderConfig
                | displayMarkers = True
                , min = min
                , max = max
                , value = initVal
                , step = step
                , onChange = onChange
            }
        ]


background w h =
    shapes [ fill (Color.rgb255 51 51 51), stroke Color.black ]
        [ rect ( 0, 0 ) w h ]


branch : Float -> Float -> Float -> Float -> Model -> List Shape
branch x y len theta model =
    let
        toX =
            -len * sin theta

        toY =
            -len * cos theta
    in
    if len > 2 then
        path ( x, y ) [ lineTo ( x + toX, y + toY ) ]
            :: branch (x + toX) (y + toY) (len * model.left_decay) (theta + degrees model.left_degrees) model
            ++ branch (x + toX) (y + toY) (len * model.right_decay) (theta + degrees -model.right_degrees) model

    else
        []



-- tree : Float -> Float -> Float -> Float -> List Shape
-- tree x y len theta =
--     []
