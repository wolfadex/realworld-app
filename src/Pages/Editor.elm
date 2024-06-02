module Pages.Editor exposing (Model, Msg, page)

import Api.Data exposing (Data)
import Auth
import Components.Editor exposing (Field, Form)
import Conduit.Api
import Conduit.Types
import Dict
import Effect exposing (Effect)
import Layouts
import OpenApi.Common
import Page exposing (Page)
import Route exposing (Route)
import Route.Path
import Shared
import View exposing (View)


page : Auth.User -> Shared.Model -> Route () -> Page Model Msg
page user shared route =
    Page.new
        { init = init shared
        , update = update route
        , subscriptions = subscriptions
        , view = view user
        }
        |> Page.withLayout (\_ -> Layouts.Default {})



-- INIT


type alias Model =
    { form : Form
    , article : Data Conduit.Types.Article
    }


init : Shared.Model -> () -> ( Model, Effect Msg )
init _ _ =
    ( { form =
            { title = ""
            , description = ""
            , body = ""
            , tags = ""
            }
      , article = Api.Data.NotAsked
      }
    , Effect.none
    )



-- UPDATE


type Msg
    = SubmittedForm Conduit.Types.User
    | Updated Field String
    | GotArticle (Result (List String) Conduit.Types.SingleArticleResponse)


update : Route () -> Msg -> Model -> ( Model, Effect Msg )
update _ msg model =
    case msg of
        Updated field value ->
            ( { model
                | form =
                    Components.Editor.updateField
                        field
                        value
                        model.form
              }
            , Effect.none
            )

        SubmittedForm user ->
            ( model
            , Conduit.Api.createArticle
                { authorization = { token = user.token }
                , body =
                    { article =
                        { title = model.form.title
                        , description = model.form.description
                        , body = model.form.body
                        , tagList =
                            model.form.tags
                                |> String.split ","
                                |> List.map String.trim
                                |> Just
                        }
                    }
                , toMsg =
                    Result.mapError
                        (\err ->
                            case Debug.log "create article error" err of
                                OpenApi.Common.KnownBadStatus _ (Conduit.Types.CreateArticle_401 _) ->
                                    [ "Please log in" ]

                                OpenApi.Common.KnownBadStatus _ (Conduit.Types.CreateArticle_422 { errors }) ->
                                    errors.body

                                _ ->
                                    [ "Failed to create article" ]
                        )
                        >> GotArticle
                }
                |> Effect.sendCmd
            )

        GotArticle response ->
            ( { model
                | article =
                    response
                        |> Result.map .article
                        |> Api.Data.fromResult
              }
            , case response of
                Ok { article } ->
                    Effect.pushRoute
                        { path = Route.Path.Article_Slug_ { slug = article.slug }
                        , query = Dict.empty
                        , hash = Nothing
                        }

                Err _ ->
                    Effect.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Conduit.Types.User -> Model -> View Msg
view user model =
    { title = "New Article"
    , body =
        [ Components.Editor.view
            { onFormSubmit = SubmittedForm user
            , title = "New Article"
            , form = model.form
            , onUpdate = Updated
            , buttonLabel = "Publish"
            , article = model.article
            }
        ]
    }
