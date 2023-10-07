module Pages.Article.Slug_ exposing (Model, Msg, page)

import Api
import Api.Article.Comment exposing (Comment)
import Api.Data exposing (Data)
import Components.IconButton as IconButton
import Dict exposing (Dict)
import Effect exposing (Effect)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, href, placeholder, src, value)
import Html.Events as Events
import Http
import Iso8601
import Layouts
import Markdown
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
    { article : Data Api.Article
    , comments : Data (List Api.Comment)
    , commentText : String
    }


init : Shared.Model -> Route { slug : String } -> () -> ( Model, Effect Msg )
init shared { params } _ =
    ( { article = Api.Data.Loading
      , comments = Api.Data.Loading
      , commentText = ""
      }
    , Effect.batch
        [ Api.getArticle
            { params = { slug = params.slug }
            , toMsg = GotArticle
            }
            |> Effect.sendCmd
        , Api.getArticleComments
            { params = { slug = params.slug }
            , toMsg = GotComments
            }
            |> Effect.sendCmd
        ]
    )



-- UPDATE


type Msg
    = GotArticle (Result Http.Error Api.SingleArticleResponse)
    | ClickedFavorite Api.User Api.Article
    | ClickedUnfavorite Api.User Api.Article
    | ClickedDeleteArticle Api.User Api.Article
    | DeletedArticle (Result Http.Error Api.EmptyOkResponse)
    | GotAuthor (Result Http.Error Api.ProfileResponse)
    | ClickedFollow Api.User Api.Profile
    | ClickedUnfollow Api.User Api.Profile
    | GotComments (Result Http.Error Api.MultipleCommentsResponse)
    | ClickedDeleteComment Api.User Api.Article Api.Comment
    | DeletedComment (Data Int)
    | SubmittedCommentForm Api.User Api.Article
    | CreatedComment (Result Http.Error Api.SingleCommentResponse)
    | UpdatedCommentText String


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        GotArticle response ->
            let
                article =
                    response
                        |> Result.mapError (\_ -> [ "Failed to get article" ])
                        |> Result.map .article
                        |> Api.Data.fromResult
            in
            ( { model | article = article }
            , Effect.none
            )

        ClickedFavorite user article ->
            ( model
            , Api.createArticleFavorite
                { authorization = { token = user.token }
                , params = { slug = article.slug }
                , toMsg = GotArticle
                }
                |> Effect.sendCmd
            )

        ClickedUnfavorite user article ->
            ( model
            , Api.deleteArticleFavorite
                { authorization = { token = user.token }
                , params = { slug = article.slug }
                , toMsg = GotArticle
                }
                |> Effect.sendCmd
            )

        ClickedDeleteArticle user article ->
            ( model
            , Api.deleteArticle
                { authorization = { token = user.token }
                , params = { slug = article.slug }
                , toMsg = DeletedArticle
                }
                |> Effect.sendCmd
            )

        DeletedArticle _ ->
            ( model
            , Effect.pushRoute
                { path = Route.Path.Home_
                , query = Dict.empty
                , hash = Nothing
                }
            )

        GotAuthor response ->
            let
                profile : Api.Data.Data Api.Profile
                profile =
                    response
                        |> Result.mapError (\_ -> [ "Failed to get author" ])
                        |> Result.map .profile
                        |> Api.Data.fromResult

                updateAuthor : Api.Article -> Api.Article
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
            , Api.followUserByUsername
                { authorization = { token = user.token }
                , params = { username = profile.username }
                , toMsg = GotAuthor
                }
                |> Effect.sendCmd
            )

        ClickedUnfollow user profile ->
            ( model
            , Api.unfollowUserByUsername
                { authorization = { token = user.token }
                , params = { username = profile.username }
                , toMsg = GotAuthor
                }
                |> Effect.sendCmd
            )

        GotComments response ->
            let
                comments =
                    response
                        |> Result.mapError (\_ -> [ "Failed to get comments" ])
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
                , Api.createArticleComment
                    { authorization = { token = user.token }
                    , params = { slug = article.slug }
                    , body = { comment = { body = model.commentText } }
                    , toMsg = CreatedComment
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
            , Api.Article.Comment.delete
                { token = user.token
                , articleSlug = article.slug
                , commentId = comment.id
                , onResponse = DeletedComment
                }
                |> Effect.sendCmd
            )

        DeletedComment id ->
            let
                removeComment : List Api.Comment -> List Api.Comment
                removeComment =
                    List.filter (\comment -> Api.Data.Success comment.id /= id)
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


viewArticle : Shared.Model -> Model -> Api.Article -> Html Msg
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


viewArticleMeta : Shared.Model -> Model -> Api.Article -> Html Msg
viewArticleMeta shared model article =
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


viewControls : Api.Article -> Api.User -> List (Html Msg)
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


viewCommentSection : Shared.Model -> Model -> Api.Article -> Html Msg
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


viewCommentForm : Model -> Api.User -> Api.Article -> Html Msg
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


viewComment : Maybe Api.User -> Api.Article -> Api.Comment -> Html Msg
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
