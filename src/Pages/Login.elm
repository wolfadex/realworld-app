module Pages.Login exposing (Model, Msg, page)

import Api.Data exposing (Data)
import Components.UserForm
import Conduit.Api
import Conduit.Types
import Dict
import Effect exposing (Effect)
import Http
import Layouts
import OpenApi.Common
import Page exposing (Page)
import Route exposing (Route)
import Route.Path
import Shared
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared req =
    Page.new
        { init = init shared
        , update = update req
        , subscriptions = subscriptions
        , view = view
        }
        |> Page.withLayout (\_ -> Layouts.Default {})



-- INIT


type alias Model =
    { user : Data Conduit.Types.User
    , email : String
    , password : String
    }


init : Shared.Model -> () -> ( Model, Effect Msg )
init shared _ =
    ( Model
        (case shared.user of
            Just user ->
                Api.Data.Success user

            Nothing ->
                Api.Data.NotAsked
        )
        ""
        ""
    , Effect.none
    )



-- UPDATE


type Msg
    = Updated Field String
    | AttemptedSignIn
    | GotUser (Result (List String) Conduit.Types.UserResponse)


type Field
    = Email
    | Password


update : Route () -> Msg -> Model -> ( Model, Effect Msg )
update _ msg model =
    case msg of
        Updated Email email ->
            ( { model | email = email }
            , Effect.none
            )

        Updated Password password ->
            ( { model | password = password }
            , Effect.none
            )

        AttemptedSignIn ->
            ( model
            , Conduit.Api.login
                { body =
                    { user =
                        { email = model.email
                        , password = model.password
                        }
                    }
                , toMsg =
                    Result.mapError
                        (\err ->
                            case err of
                                OpenApi.Common.KnownBadStatus _ (Conduit.Types.Login_422 { errors }) ->
                                    errors.body

                                _ ->
                                    [ "Failed to login" ]
                        )
                        >> GotUser
                }
                |> Effect.sendCmd
            )

        GotUser response ->
            let
                user =
                    response
                        |> Result.map .user
                        |> Api.Data.fromResult
            in
            case Api.Data.toMaybe user of
                Just user_ ->
                    ( { model | user = user }
                    , Effect.batch
                        [ Effect.pushRoute
                            { path = Route.Path.Home_
                            , query = Dict.empty
                            , hash = Nothing
                            }
                        , Effect.signIn user_
                        ]
                    )

                Nothing ->
                    ( { model | user = user }
                    , Effect.none
                    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Sign in"
    , body =
        [ Components.UserForm.view
            { user = model.user
            , label = "Sign in"
            , onFormSubmit = AttemptedSignIn
            , alternateLink =
                { label = "Need an account?"
                , route = Route.Path.Register
                }
            , fields =
                [ { label = "Email"
                  , type_ = "email"
                  , value = model.email
                  , onInput = Updated Email
                  }
                , { label = "Password"
                  , type_ = "password"
                  , value = model.password
                  , onInput = Updated Password
                  }
                ]
            }
        ]
    }
