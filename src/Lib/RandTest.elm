module Lib.RandTest exposing (..)

import Array exposing (..)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random exposing (..)
import Random.Array as RandArr exposing (..)



-- Model


type alias Model =
    Array Int


init : flags -> ( Model, Cmd Msg )
init _ =
    ( Array.empty, Cmd.none )



-- Update


type Msg
    = Clicked
    | MakeRandomArray (Array Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Clicked ->
            ( model, makeRandArr )

        MakeRandomArray newModel ->
            ( newModel, Cmd.none )


makeRandArr : Cmd Msg
makeRandArr =
    Random.generate MakeRandomArray (RandArr.array 1000 (int 0 100))



-- Subscription


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- View


view : Model -> Html Msg
view model =
    div [ style "margin" "20px" ]
        [ div [] [ button [ onClick Clicked ] [ text "Roll 1000 times!" ] ]
        , div [] <| Array.toList (Array.map (\i -> text (String.fromInt i ++ ", ")) model)
        , div [ style "padding-top" "10px" ]
            [ text <|
                (++) "Average is " <|
                    String.fromInt <|
                        (//) (Array.foldr (+) 0 model) 1000
            ]
        ]



-- Main


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
