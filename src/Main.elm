module Main exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Browser.Events
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy as Lazy exposing (..)
import Lib.Ripple as Ripple
import Material.Drawer as Drawer exposing (..)
import Material.IconButton as IB exposing (iconButtonConfig)
import Material.List exposing (..)
import Material.TopAppBar as TAB exposing (topAppBarConfig)
import Maybe.Extra exposing (..)
import Random exposing (..)
import Task exposing (..)
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
    , isDrawerOpen : Bool

    -- Ripple
    , ripples : List Ripple.Ripple
    }



-- Update


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url
    | UpdateFrame Float
    | DrawerClosed
    | DrawerOpend
    | RMsg Ripple.Msg
    | CreateRipple ( Float, Float ) Float


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model
        key
        url
        0
        0
        False
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

        DrawerClosed ->
            ( { model | isDrawerOpen = False }, Cmd.none )

        DrawerOpend ->
            ( { model | isDrawerOpen = True }, Cmd.none )

        UpdateFrame dt ->
            ( { model
                | dt = dt
                , ripples = Ripple.update model.ripples dt
              }
            , Cmd.none
            )

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
    if model.isDrawerOpen then
        Sub.none

    else
        Browser.Events.onAnimationFrameDelta UpdateFrame


view : Model -> Browser.Document Msg
view model =
    let
        mainContent =
            routes model
    in
    { title = model.url.path
    , body =
        [ appBar model
        , drawer model.isDrawerOpen
            [ div
                [ style "padding-top" "128px" ]
                [ text "New Application" ]
            ]
        , h1 [] [ text (Url.toString model.url) ]
        , h1 [] [ text ("update per millisec" ++ String.fromFloat model.dt) ]
        , mainContent
        ]
    }


appBar : Model -> Html Msg
appBar model =
    TAB.topAppBar { topAppBarConfig | fixed = False }
        [ TAB.row []
            [ TAB.section [ TAB.alignStart ]
                [ IB.iconButton
                    { iconButtonConfig
                        | onClick = Just DrawerOpend
                        , additionalAttributes =
                            [ TAB.navigationIcon ]
                    }
                    "menu"
                , span [ TAB.title ] [ text "title" ]
                ]
            ]
        ]



-- Routing


routes : Model -> Html Msg
routes model =
    case model.url.path of
        "/ripple" ->
            Html.map RMsg (Ripple.view model.ripples)

        "/world" ->
            h1 [] [ text "world" ]

        _ ->
            h1 [] [ text "Nothing" ]


drawer : Bool -> List (Html Msg) -> Html Msg
drawer isOpen mainContent =
    div []
        [ dismissibleDrawer
            { dismissibleDrawerConfig
                | open = isOpen
                , onClose = Nothing
                , additionalAttributes = []
            }
            [ drawerContent [] [ drawerList ] ]
        , drawerScrim [ onClick DrawerClosed ] []
        , div [] mainContent
        ]


drawerList : Html Msg
drawerList =
    Material.List.list listConfig
        [ listItem { listItemConfig | href = Just "/ripple", onClick = Just DrawerClosed } [ text "Ripple" ]
        , listItem { listItemConfig | href = Just "/world", onClick = Just DrawerClosed } [ text "world" ]
        ]
