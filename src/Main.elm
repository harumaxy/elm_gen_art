module Main exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Browser.Dom exposing (..)
import Browser.Events
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy as Lazy exposing (..)
import Lib.Branch as Branch exposing (..)
import Lib.Fractals exposing (..)
import Lib.RecursiveLine exposing (..)
import Lib.Ripple as Ripple
import Material.Drawer as Drawer exposing (..)
import Material.IconButton as IB exposing (iconButtonConfig)
import Material.LayoutGrid as Grid exposing (..)
import Material.List exposing (..)
import Material.TopAppBar as TAB exposing (topAppBarConfig)
import Maybe.Extra exposing (..)
import Random exposing (..)
import String.Extra exposing (..)
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



-- MODEL


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , pageTitle : String
    , time : Float
    , dt : Float
    , isDrawerOpen : Bool
    , viewport : Viewport

    -- Ripple
    , ripples : List Ripple.Ripple

    -- Branch
    , branch : Branch.Model
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model
        key
        url
        "Home"
        0
        0
        False
        (Viewport
            { width = 0
            , height = 0
            }
            { x = 0
            , y = 0
            , width = 0
            , height = 0
            }
        )
        []
        Branch.init
    , Task.perform GetViewport Browser.Dom.getViewport
    )



-- Update


type Msg
    = GetViewport Viewport
    | UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url
    | UpdateFrame Float
    | DrawerClosed
    | DrawerOpend
    | RMsg Ripple.Msg
    | CreateRipple ( Float, Float ) Float
    | BMsg Branch.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Main
        GetViewport newViewport ->
            ( { model | viewport = newViewport }, Cmd.none )

        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url, pageTitle = String.Extra.toTitleCase (String.dropLeft 1 url.path) }
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

        -- Branch
        BMsg bmsg ->
            ( { model | branch = Branch.update bmsg model.branch }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.url.path == "/ripple" then
        Browser.Events.onAnimationFrameDelta UpdateFrame

    else
        Sub.none


view : Model -> Browser.Document Msg
view model =
    { title = model.pageTitle
    , body =
        [ Grid.layoutGrid [ style "margin" "0px", style "padding" "0px" ]
            [ appBar model
            , drawer model.isDrawerOpen
                [ div [ style "padding-top" "128px" ] [] ]
            , routes model
            ]
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
                , span [ TAB.title ] [ text model.pageTitle ]
                ]
            ]
        ]



-- Routing


routes : Model -> Html Msg
routes model =
    case model.url.path of
        "/ripple" ->
            Html.map RMsg (Ripple.view model.ripples model.viewport.scene)

        "/fractals" ->
            Lib.Fractals.view

        "/recursiveLine" ->
            Lib.RecursiveLine.view model.viewport.scene

        "/recursiveBranch" ->
            Html.map BMsg (Branch.view model.branch)

        _ ->
            Html.map BMsg (Branch.view model.branch)


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
        , listItem { listItemConfig | href = Just "/fractals", onClick = Just DrawerClosed } [ text "Fractals" ]
        , listItem { listItemConfig | href = Just "/recursiveLine", onClick = Just DrawerClosed } [ text "RecursiveLine" ]
        , listItem { listItemConfig | href = Just "/recursiveBranch", onClick = Just DrawerClosed } [ text "RecursiveBranch" ]
        ]
