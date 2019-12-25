module Main exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Browser.Events
import Browser.Navigation as Nav
import Html exposing (..)
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
    , dt : Float
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model key url 0, Cmd.none )


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url
    | UpdateFrame Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
            ( { model | dt = dt }, Cmd.none )


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
        ]
    }
