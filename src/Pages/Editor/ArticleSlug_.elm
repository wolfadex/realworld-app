module Pages.Editor.ArticleSlug_ exposing (Model, Msg, page)

import Api.Data exposing (Data)
import Auth
import Components.Editor exposing (Field, Form)
import Conduit.Api
import Conduit.Types
import Dict
import Effect exposing (Effect)
import Html exposing (..)
import Layouts
import OpenApi.Common
import Page exposing (Page)
import Route exposing (Route)
import Route.Path
import Shared
import View exposing (View)


page : Auth.User -> Shared.Model -> Route { articleSlug : String } -> Page Model Msg
page user shared route =
    Page.new
        { init = init shared route
        , update = update route
        , subscriptions = subscriptions
        , view = view user
        }
        |> Page.withLayout (\_ -> Layouts.Default {})



-- INIT


type alias Model =
    { slug : String
    , form : Maybe Form
    , article : Data Conduit.Types.Article
    }


init : Shared.Model -> Route { articleSlug : String } -> () -> ( Model, Effect Msg )
init _ { params } _ =
    ( { slug = params.articleSlug
      , form = Nothing
      , article = Api.Data.Loading
      }
    , Conduit.Api.getArticle
        { params = { slug = params.articleSlug }
        , toMsg =
            Result.mapError
                (\err ->
                    case err of
                        OpenApi.Common.KnownBadStatus _ (Conduit.Types.GetArticle_422 { errors }) ->
                            errors.body

                        _ ->
                            [ "Failed to get article" ]
                )
                >> LoadedInitialArticle
        }
        |> Effect.sendCmd
    )



-- UPDATE


type Msg
    = SubmittedForm Conduit.Types.User Form
    | Updated Field String
    | UpdatedArticle (Result (List String) Conduit.Types.SingleArticleResponse)
    | LoadedInitialArticle (Result (List String) Conduit.Types.SingleArticleResponse)


update : Route { articleSlug : String } -> Msg -> Model -> ( Model, Effect Msg )
update _ msg model =
    case msg of
        LoadedInitialArticle response ->
            case response of
                Ok { article } ->
                    ( { model
                        | form =
                            Just <|
                                { title = article.title
                                , description = article.description
                                , body = article.body
                                , tags = String.join ", " article.tagList
                                }
                      }
                    , Effect.none
                    )

                Err _ ->
                    ( model, Effect.none )

        Updated field value ->
            ( { model
                | form =
                    Maybe.map
                        (Components.Editor.updateField field value)
                        model.form
              }
            , Effect.none
            )

        SubmittedForm user form ->
            ( model
            , Conduit.Api.updateArticle
                { authorization = { token = user.token }
                , params = { slug = model.slug }
                , body =
                    { article =
                        { body = Just form.body
                        , description = Just form.description
                        , title = Just form.title
                        }
                    }
                , toMsg =
                    Result.mapError
                        (\err ->
                            case err of
                                OpenApi.Common.KnownBadStatus _ (Conduit.Types.UpdateArticle_401 _) ->
                                    [ "Please log in" ]

                                OpenApi.Common.KnownBadStatus _ (Conduit.Types.UpdateArticle_422 { errors }) ->
                                    errors.body

                                _ ->
                                    [ "Failed to update article" ]
                        )
                        >> UpdatedArticle
                }
                |> Effect.sendCmd
            )

        UpdatedArticle response ->
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
    { title = "Editing Article"
    , body =
        case model.form of
            Just form ->
                [ Components.Editor.view
                    { onFormSubmit = SubmittedForm user form
                    , title = "Edit Article"
                    , form = form
                    , onUpdate = Updated
                    , buttonLabel = "Save"
                    , article = model.article
                    }
                ]

            Nothing ->
                []
    }
