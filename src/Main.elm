module Main exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Browser.Events
import Browser.Navigation as Nav
import Html exposing (..)
import Lib.Ripple as Ripple
import Random exposing (..)
import Url


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , time : Float
    , dt : Float

    -- Ripple
    , ripples : List Ripple.Ripple
    }



-- Update


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url
    | UpdateFrame Float
    | RMsg Ripple.Msg
    | CreateRipple ( Float, Float ) Float


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model
        key
        url
        0
        0
        []
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Main
        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )

        UpdateFrame dt ->
            ( { model | dt = dt, ripples = Ripple.update model.ripples dt }, Cmd.none )

        -- Ripple
        RMsg rmsg ->
            let
                maxRound =
                    200

                minRound =
                    50
            in
            case rmsg of
                Ripple.ClickedAt pos ->
                    ( model, Random.generate (CreateRipple pos) (Random.float minRound maxRound) )

        CreateRipple pos r ->
            ( { model | ripples = Ripple.Ripple pos 0 r :: model.ripples }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onAnimationFrameDelta UpdateFrame


view : Model -> Browser.Document Msg
view model =
    { title = "Application Title"
    , body =
        [ div [] [ text "New Application" ]
        , h1 [] [ text (Url.toString model.url) ]
        , h1 [] [ text ("update per millisec" ++ String.fromFloat model.dt) ]
        , Html.map RMsg (Ripple.view model.ripples)
        ]
    }
