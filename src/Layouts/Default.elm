module Layouts.Default exposing (Model, Msg, Props, layout)

import Components.Footer
import Components.Navbar
import Effect exposing (Effect)
import Html exposing (..)
import Html.Attributes exposing (class)
import Layout exposing (Layout)
import Route exposing (Route)
import Shared
import View exposing (View)


type alias Props =
    {}


layout : Props -> Shared.Model -> Route () -> Layout () Model Msg contentMsg
layout _ shared route =
    Layout.new
        { init = init
        , update = update
        , view = view shared route
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    {}


init : () -> ( Model, Effect Msg )
init _ =
    ( {}
    , Effect.none
    )



-- UPDATE


type Msg
    = ClickedSignOut


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ClickedSignOut ->
            ( model
            , Effect.signOut
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Shared.Model -> Route () -> { toContentMsg : Msg -> contentMsg, content : View contentMsg, model : Model } -> View contentMsg
view shared route { toContentMsg, content } =
    { title =
        if String.isEmpty content.title then
            "Conduit"

        else
            content.title ++ " | Conduit"
    , body =
        [ div [ class "layout" ]
            [ Components.Navbar.view
                { user = shared.user
                , currentRoutePath = route.path
                , onSignOut = toContentMsg ClickedSignOut
                }
            , div [ class "page" ] content.body
            , Components.Footer.view
            ]
        ]
    }
