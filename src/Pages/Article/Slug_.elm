module Pages.Article.Slug_ exposing (Model, Msg, page)

import Api.Data exposing (Data)
import Components.IconButton as IconButton
import Conduit.Api
import Conduit.Types
import Dict
import Effect exposing (Effect)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, href, placeholder, src, value)
import Html.Events as Events
import Iso8601
import Layouts
import Markdown
import OpenApi.Common
import Page exposing (Page)
import Route exposing (Route)
import Route.Path
import Shared
import Utils.Maybe
import Utils.Time
import View exposing (View)


page : Shared.Model -> Route { slug : String } -> Page Model Msg
page shared route =
    Page.new
        { init = init shared route
        , update = update
        , subscriptions = subscriptions
        , view = view shared
        }
        |> Page.withLayout (\_ -> Layouts.Default {})



-- INIT


type alias Model =
    { article : Data Conduit.Types.Article
    , comments : Data (List Conduit.Types.Comment)
    , commentText : String
    }


init : Shared.Model -> Route { slug : String } -> () -> ( Model, Effect Msg )
init _ { params } _ =
    ( { article = Api.Data.Loading
      , comments = Api.Data.Loading
      , commentText = ""
      }
    , Effect.batch
        [ Conduit.Api.getArticle
            { params = { slug = params.slug }
            , toMsg =
                Result.mapError
                    (\err ->
                        case err of
                            OpenApi.Common.KnownBadStatus _ (Conduit.Types.GetArticle_422 { errors }) ->
                                errors.body

                            _ ->
                                [ "Failed to get article" ]
                    )
                    >> GotArticle
            }
            |> Effect.sendCmd
        , Conduit.Api.getArticleComments
            { params = { slug = params.slug }
            , toMsg =
                Result.mapError
                    (\err ->
                        case err of
                            OpenApi.Common.KnownBadStatus _ (Conduit.Types.GetArticleComments_401 _) ->
                                [ "Please log in" ]

                            OpenApi.Common.KnownBadStatus _ (Conduit.Types.GetArticleComments_422 { errors }) ->
                                errors.body

                            _ ->
                                [ "Failed to get article" ]
                    )
                    >> GotComments
            }
            |> Effect.sendCmd
        ]
    )



-- UPDATE


type Msg
    = GotArticle (Result (List String) Conduit.Types.SingleArticleResponse)
    | ClickedFavorite Conduit.Types.User Conduit.Types.Article
    | ClickedUnfavorite Conduit.Types.User Conduit.Types.Article
    | ClickedDeleteArticle Conduit.Types.User Conduit.Types.Article
    | DeletedArticle
    | GotAuthor (Result (List String) Conduit.Types.ProfileResponse)
    | ClickedFollow Conduit.Types.User Conduit.Types.Profile
    | ClickedUnfollow Conduit.Types.User Conduit.Types.Profile
    | GotComments (Result (List String) Conduit.Types.MultipleCommentsResponse)
    | ClickedDeleteComment Conduit.Types.User Conduit.Types.Article Conduit.Types.Comment
    | DeletedComment Int
    | SubmittedCommentForm Conduit.Types.User Conduit.Types.Article
    | CreatedComment (Result (List String) Conduit.Types.SingleCommentResponse)
    | UpdatedCommentText String


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        GotArticle response ->
            let
                article =
                    response
                        |> Result.map .article
                        |> Api.Data.fromResult
            in
            ( { model | article = article }
            , Effect.none
            )

        ClickedFavorite user article ->
            ( model
            , Conduit.Api.createArticleFavorite
                { authorization = { token = user.token }
                , params = { slug = article.slug }
                , toMsg =
                    Result.mapError
                        (\err ->
                            case err of
                                OpenApi.Common.KnownBadStatus _ (Conduit.Types.CreateArticleFavorite_401 _) ->
                                    [ "Please log in" ]

                                OpenApi.Common.KnownBadStatus _ (Conduit.Types.CreateArticleFavorite_422 { errors }) ->
                                    errors.body

                                _ ->
                                    [ "Failed to favorite article" ]
                        )
                        >> GotArticle
                }
                |> Effect.sendCmd
            )

        ClickedUnfavorite user article ->
            ( model
            , Conduit.Api.deleteArticleFavorite
                { authorization = { token = user.token }
                , params = { slug = article.slug }
                , toMsg =
                    Result.mapError
                        (\err ->
                            case err of
                                OpenApi.Common.KnownBadStatus _ (Conduit.Types.DeleteArticleFavorite_401 _) ->
                                    [ "Please log in" ]

                                OpenApi.Common.KnownBadStatus _ (Conduit.Types.DeleteArticleFavorite_422 { errors }) ->
                                    errors.body

                                _ ->
                                    [ "Failed to unfavorite article" ]
                        )
                        >> GotArticle
                }
                |> Effect.sendCmd
            )

        ClickedDeleteArticle user article ->
            ( model
            , Conduit.Api.deleteArticle
                { authorization = { token = user.token }
                , params = { slug = article.slug }
                , toMsg = \_ -> DeletedArticle
                }
                |> Effect.sendCmd
            )

        DeletedArticle ->
            ( model
            , Effect.pushRoute
                { path = Route.Path.Home_
                , query = Dict.empty
                , hash = Nothing
                }
            )

        GotAuthor response ->
            let
                profile : Api.Data.Data Conduit.Types.Profile
                profile =
                    response
                        |> Result.map .profile
                        |> Api.Data.fromResult

                updateAuthor : Conduit.Types.Article -> Conduit.Types.Article
                updateAuthor article =
                    case profile of
                        Api.Data.Success author ->
                            { article | author = author }

                        _ ->
                            article
            in
            ( { model | article = Api.Data.map updateAuthor model.article }
            , Effect.none
            )

        ClickedFollow user profile ->
            ( model
            , Conduit.Api.followUserByUsername
                { authorization = { token = user.token }
                , params = { username = profile.username }
                , toMsg =
                    Result.mapError
                        (\err ->
                            case err of
                                OpenApi.Common.KnownBadStatus _ (Conduit.Types.FollowUserByUsername_401 _) ->
                                    [ "Please log in" ]

                                OpenApi.Common.KnownBadStatus _ (Conduit.Types.FollowUserByUsername_422 { errors }) ->
                                    errors.body

                                _ ->
                                    [ "Failed to follow user" ]
                        )
                        >> GotAuthor
                }
                |> Effect.sendCmd
            )

        ClickedUnfollow user profile ->
            ( model
            , Conduit.Api.unfollowUserByUsername
                { authorization = { token = user.token }
                , params = { username = profile.username }
                , toMsg =
                    Result.mapError
                        (\err ->
                            case err of
                                OpenApi.Common.KnownBadStatus _ (Conduit.Types.UnfollowUserByUsername_401 _) ->
                                    [ "Please log in" ]

                                OpenApi.Common.KnownBadStatus _ (Conduit.Types.UnfollowUserByUsername_422 { errors }) ->
                                    errors.body

                                _ ->
                                    [ "Failed to unfollow user" ]
                        )
                        >> GotAuthor
                }
                |> Effect.sendCmd
            )

        GotComments response ->
            let
                comments =
                    response
                        |> Result.map .comments
                        |> Api.Data.fromResult
            in
            ( { model | comments = comments }
            , Effect.none
            )

        UpdatedCommentText text ->
            ( { model | commentText = text }
            , Effect.none
            )

        SubmittedCommentForm user article ->
            if String.isEmpty model.commentText then
                ( model, Effect.none )

            else
                ( { model | commentText = "" }
                , Conduit.Api.createArticleComment
                    { authorization = { token = user.token }
                    , params = { slug = article.slug }
                    , body = { comment = { body = model.commentText } }
                    , toMsg =
                        Result.mapError
                            (\err ->
                                case err of
                                    OpenApi.Common.KnownBadStatus _ (Conduit.Types.CreateArticleComment_401 _) ->
                                        [ "Please log in" ]

                                    OpenApi.Common.KnownBadStatus _ (Conduit.Types.CreateArticleComment_422 { errors }) ->
                                        errors.body

                                    _ ->
                                        [ "Failed to comment" ]
                            )
                            >> CreatedComment
                    }
                    |> Effect.sendCmd
                )

        CreatedComment response ->
            case response of
                Ok { comment } ->
                    ( { model | comments = Api.Data.map (\comments -> comment :: comments) model.comments }
                    , Effect.none
                    )

                Err _ ->
                    ( model, Effect.none )

        ClickedDeleteComment user article comment ->
            ( model
            , Conduit.Api.deleteArticleComment
                { authorization = { token = user.token }
                , params =
                    { slug = article.slug
                    , id = comment.id
                    }
                , toMsg = \_ -> DeletedComment comment.id
                }
                |> Effect.sendCmd
            )

        DeletedComment id ->
            let
                removeComment : List Conduit.Types.Comment -> List Conduit.Types.Comment
                removeComment =
                    List.filter (\comment -> comment.id /= id)
            in
            ( { model | comments = Api.Data.map removeComment model.comments }
            , Effect.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    case model.article of
        Api.Data.Success article ->
            { title = article.title
            , body = [ viewArticle shared model article ]
            }

        _ ->
            { title = "Article"
            , body = []
            }


viewArticle : Shared.Model -> Model -> Conduit.Types.Article -> Html Msg
viewArticle shared model article =
    div [ class "article-page" ]
        [ div [ class "banner" ]
            [ div [ class "container" ]
                [ h1 [] [ text article.title ]
                , viewArticleMeta shared model article
                ]
            ]
        , div [ class "container page" ]
            [ div [ class "row article-content" ]
                [ div [ class "col-md-12" ]
                    [ Markdown.toHtml [] article.body ]
                , if List.isEmpty article.tagList then
                    text ""

                  else
                    ul [ class "tag-list" ]
                        (List.map
                            (\tag -> li [ class "tag-default tag-pill tag-outline" ] [ text tag ])
                            article.tagList
                        )
                ]
            , hr [] []
            , div [ class "article-actions" ] [ viewArticleMeta shared model article ]
            , viewCommentSection shared model article
            ]
        ]


viewArticleMeta : Shared.Model -> Model -> Conduit.Types.Article -> Html Msg
viewArticleMeta shared _ article =
    div [ class "article-meta" ] <|
        List.concat
            [ [ a [ href ("/profile/" ++ article.author.username) ]
                    [ img [ src article.author.image ] []
                    ]
              , div [ class "info" ]
                    [ a [ class "author", href ("/profile/" ++ article.author.username) ] [ text article.author.username ]
                    , span [ class "date" ]
                        [ article.createdAt
                            |> Iso8601.toTime
                            |> Result.map Utils.Time.formatDate
                            |> Result.withDefault article.createdAt
                            |> text
                        ]
                    ]
              ]
            , case shared.user of
                Just user ->
                    viewControls article user

                Nothing ->
                    []
            ]


viewControls : Conduit.Types.Article -> Conduit.Types.User -> List (Html Msg)
viewControls article user =
    if article.author.username == user.username then
        [ a
            [ class "btn btn-outline-secondary btn-sm"
            , href ("/editor/" ++ article.slug)
            ]
            [ i [ class "ion-edit" ] []
            , text "Edit article"
            ]
        , IconButton.view
            { color = IconButton.OutlinedRed
            , icon = IconButton.Trash
            , label = "Delete article"
            , onClick = ClickedDeleteArticle user article
            }
        ]

    else
        [ if article.author.following then
            IconButton.view
                { color = IconButton.FilledGray
                , icon = IconButton.Plus
                , label = "Unfollow " ++ article.author.username
                , onClick = ClickedUnfollow user article.author
                }

          else
            IconButton.view
                { color = IconButton.OutlinedGray
                , icon = IconButton.Plus
                , label = "Follow " ++ article.author.username
                , onClick = ClickedFollow user article.author
                }
        , if article.favorited then
            IconButton.view
                { color = IconButton.FilledGreen
                , icon = IconButton.Heart
                , label = "Unfavorite Post (" ++ String.fromInt article.favoritesCount ++ ")"
                , onClick = ClickedUnfavorite user article
                }

          else
            IconButton.view
                { color = IconButton.OutlinedGreen
                , icon = IconButton.Heart
                , label = "Favorite Post (" ++ String.fromInt article.favoritesCount ++ ")"
                , onClick = ClickedFavorite user article
                }
        ]


viewCommentSection : Shared.Model -> Model -> Conduit.Types.Article -> Html Msg
viewCommentSection shared model article =
    div [ class "row" ]
        [ div [ class "col-xs-12 col-md-8 offset-md-2" ] <|
            List.concat
                [ case shared.user of
                    Just user ->
                        [ viewCommentForm model user article ]

                    Nothing ->
                        []
                , case model.comments of
                    Api.Data.Success comments ->
                        List.map (viewComment shared.user article) comments

                    _ ->
                        []
                ]
        ]


viewCommentForm : Model -> Conduit.Types.User -> Conduit.Types.Article -> Html Msg
viewCommentForm model user article =
    form [ class "card comment-form", Events.onSubmit (SubmittedCommentForm user article) ]
        [ div [ class "card-block" ]
            [ textarea
                [ class "form-control"
                , placeholder "Write a comment..."
                , attribute "rows" "3"
                , value model.commentText
                , Events.onInput UpdatedCommentText
                ]
                []
            ]
        , div [ class "card-footer" ]
            [ img [ class "comment-author-img", src user.image ] []
            , button [ class "btn btn-sm btn-primary" ] [ text "Post Comment" ]
            ]
        ]


viewComment : Maybe Conduit.Types.User -> Conduit.Types.Article -> Conduit.Types.Comment -> Html Msg
viewComment currentUser article comment =
    let
        viewCommentActions =
            Utils.Maybe.view currentUser <|
                \user ->
                    if user.username == comment.author.username then
                        span
                            [ class "mod-options"
                            , Events.onClick (ClickedDeleteComment user article comment)
                            ]
                            [ i [ class "ion-trash-a" ] [] ]

                    else
                        text ""
    in
    div [ class "card" ]
        [ div [ class "card-block" ]
            [ p [ class "card-text" ] [ text comment.body ] ]
        , div [ class "card-footer" ]
            [ a
                [ class "comment-author"
                , href ("/profile/" ++ comment.author.username)
                ]
                [ img [ class "comment-author-img", src comment.author.image ] []
                , text comment.author.username
                ]
            , span [ class "date-posted" ]
                [ comment.createdAt
                    |> Iso8601.toTime
                    |> Result.map Utils.Time.formatDate
                    |> Result.withDefault comment.createdAt
                    |> text
                ]
            , viewCommentActions
            ]
        ]
