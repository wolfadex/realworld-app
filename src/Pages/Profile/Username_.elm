module Pages.Profile.Username_ exposing (Model, Msg, Tab, page)

import Api
import Api.Data exposing (Data)
import Article
import Components.ArticleList
import Components.IconButton as IconButton
import Components.NotFound
import Effect exposing (Effect)
import Html exposing (..)
import Html.Attributes exposing (class, classList, src)
import Html.Events as Events
import Http
import Layouts
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Utils.Maybe
import View exposing (View)


page : Shared.Model -> Route { username : String } -> Page Model Msg
page shared req =
    Page.new
        { init = init shared req
        , update = update shared
        , subscriptions = subscriptions
        , view = view shared
        }
        |> Page.withLayout (\_ -> Layouts.Default {})



-- INIT


type alias Model =
    { username : String
    , profile : Data Api.Profile
    , listing : Data Article.Listing
    , selectedTab : Tab
    , page : Int
    }


type Tab
    = MyArticles
    | FavoritedArticles


init : Shared.Model -> Route { username : String } -> () -> ( Model, Effect Msg )
init _ { params } _ =
    ( { username = params.username
      , profile = Api.Data.Loading
      , listing = Api.Data.Loading
      , selectedTab = MyArticles
      , page = 1
      }
    , Effect.batch
        [ Api.getProfileByUsername
            { params = params
            , toMsg =
                Result.mapError
                    (\err ->
                        case err of
                            Api.KnownBadStatus _ (Api.GetProfileByUsername_401 _) ->
                                [ "Please log in" ]

                            Api.KnownBadStatus _ (Api.GetProfileByUsername_422 { errors }) ->
                                errors.body

                            _ ->
                                [ "Failed to load user profile" ]
                    )
                    >> GotProfile
            }
            |> Effect.sendCmd
        , fetchArticlesBy params.username 1
        ]
    )


pageLimit : Int
pageLimit =
    25


fetchArticlesBy : String -> Int -> Effect Msg
fetchArticlesBy username page_ =
    Api.getArticles
        { params =
            { tag = Nothing
            , author = Just username
            , favorited = Nothing
            , offset = Just ((page_ - 1) * pageLimit)
            , limit = Just pageLimit
            }
        , toMsg =
            Result.mapError
                (\err ->
                    case err of
                        Api.KnownBadStatus _ (Api.GetArticles_401 _) ->
                            [ "Please log in" ]

                        Api.KnownBadStatus _ (Api.GetArticles_422 { errors }) ->
                            errors.body

                        _ ->
                            [ "Failed to get articles" ]
                )
                >> GotArticles page_
        }
        |> Effect.sendCmd


fetchArticlesFavoritedBy : String -> Int -> Effect Msg
fetchArticlesFavoritedBy username page_ =
    Api.getArticles
        { params =
            { tag = Nothing
            , author = Nothing
            , favorited = Just username
            , offset = Just ((page_ - 1) * pageLimit)
            , limit = Just pageLimit
            }
        , toMsg =
            Result.mapError
                (\err ->
                    case err of
                        Api.KnownBadStatus _ (Api.GetArticles_401 _) ->
                            [ "Please log in" ]

                        Api.KnownBadStatus _ (Api.GetArticles_422 { errors }) ->
                            errors.body

                        _ ->
                            [ "Failed to get articles" ]
                )
                >> GotArticles page_
        }
        |> Effect.sendCmd



-- UPDATE


type Msg
    = GotProfile (Result (List String) Api.ProfileResponse)
    | GotArticles Int (Result (List String) Api.MultipleArticlesResponse)
    | Clicked Tab
    | ClickedFavorite Api.User Api.Article
    | ClickedUnfavorite Api.User Api.Article
    | UpdatedArticle (Result (List String) Api.SingleArticleResponse)
    | ClickedFollow Api.User Api.Profile
    | ClickedUnfollow Api.User Api.Profile
    | ClickedPage Int


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update _ msg model =
    case msg of
        GotProfile profile ->
            ( { model
                | profile =
                    profile
                        |> Result.map .profile
                        |> Api.Data.fromResult
              }
            , Effect.none
            )

        ClickedFollow user profile ->
            ( model
            , Api.followUserByUsername
                { authorization = { token = user.token }
                , params = { username = profile.username }
                , toMsg =
                    Result.mapError
                        (\err ->
                            case err of
                                Api.KnownBadStatus _ (Api.FollowUserByUsername_401 _) ->
                                    [ "Please log in" ]

                                Api.KnownBadStatus _ (Api.FollowUserByUsername_422 { errors }) ->
                                    errors.body

                                _ ->
                                    [ "Failed to follow user" ]
                        )
                        >> GotProfile
                }
                |> Effect.sendCmd
            )

        ClickedUnfollow user profile ->
            ( model
            , Api.unfollowUserByUsername
                { authorization = { token = user.token }
                , params = { username = profile.username }
                , toMsg =
                    Result.mapError
                        (\err ->
                            case err of
                                Api.KnownBadStatus _ (Api.UnfollowUserByUsername_401 _) ->
                                    [ "Please log in" ]

                                Api.KnownBadStatus _ (Api.UnfollowUserByUsername_422 { errors }) ->
                                    errors.body

                                _ ->
                                    [ "Failed to unfollow user" ]
                        )
                        >> GotProfile
                }
                |> Effect.sendCmd
            )

        GotArticles page_ response ->
            ( { model
                | listing =
                    response
                        |> Result.map
                            (\{ articles, articlesCount } ->
                                { articles = articles
                                , page = page_
                                , totalPages = articlesCount // pageLimit
                                }
                            )
                        |> Api.Data.fromResult
              }
            , Effect.none
            )

        Clicked MyArticles ->
            ( { model
                | selectedTab = MyArticles
                , listing = Api.Data.Loading
                , page = 1
              }
            , fetchArticlesBy model.username 1
            )

        Clicked FavoritedArticles ->
            ( { model
                | selectedTab = FavoritedArticles
                , listing = Api.Data.Loading
                , page = 1
              }
            , fetchArticlesFavoritedBy model.username 1
            )

        ClickedFavorite user article ->
            ( model
            , Api.createArticleFavorite
                { authorization = { token = user.token }
                , params = { slug = article.slug }
                , toMsg =
                    Result.mapError
                        (\err ->
                            case err of
                                Api.KnownBadStatus _ (Api.CreateArticleFavorite_401 _) ->
                                    [ "Please log in" ]

                                Api.KnownBadStatus _ (Api.CreateArticleFavorite_422 { errors }) ->
                                    errors.body

                                _ ->
                                    [ "Failed to favorite article" ]
                        )
                        >> UpdatedArticle
                }
                |> Effect.sendCmd
            )

        ClickedUnfavorite user article ->
            ( model
            , Api.deleteArticleFavorite
                { authorization = { token = user.token }
                , params = { slug = article.slug }
                , toMsg =
                    Result.mapError
                        (\err ->
                            case err of
                                Api.KnownBadStatus _ (Api.DeleteArticleFavorite_401 _) ->
                                    [ "Please log in" ]

                                Api.KnownBadStatus _ (Api.DeleteArticleFavorite_422 { errors }) ->
                                    errors.body

                                _ ->
                                    [ "Failed to unfavorite article" ]
                        )
                        >> UpdatedArticle
                }
                |> Effect.sendCmd
            )

        ClickedPage page_ ->
            let
                fetch : String -> Int -> Effect Msg
                fetch =
                    case model.selectedTab of
                        MyArticles ->
                            fetchArticlesBy

                        FavoritedArticles ->
                            fetchArticlesFavoritedBy
            in
            ( { model
                | listing = Api.Data.Loading
                , page = page_
              }
            , fetch
                model.username
                page_
            )

        UpdatedArticle (Ok { article }) ->
            ( { model
                | listing =
                    Api.Data.map (Article.updateListing article)
                        model.listing
              }
            , Effect.none
            )

        UpdatedArticle (Err _) ->
            ( model, Effect.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Profile"
    , body =
        case model.profile of
            Api.Data.Success profile ->
                [ viewProfile shared profile model ]

            Api.Data.Failure _ ->
                [ Components.NotFound.view ]

            _ ->
                []
    }


viewProfile : Shared.Model -> Api.Profile -> Model -> Html Msg
viewProfile shared profile model =
    let
        isViewingOwnProfile : Bool
        isViewingOwnProfile =
            Maybe.map .username shared.user == Just profile.username

        viewUserInfo : Html Msg
        viewUserInfo =
            div [ class "user-info" ]
                [ div [ class "container" ]
                    [ div [ class "row" ]
                        [ div [ class "col-xs-12 col-md-10 offset-md-1" ]
                            [ img [ class "user-img", src profile.image ] []
                            , h4 [] [ text profile.username ]
                            , -- case profile.bio of
                              --     Nothing ->
                              --         text ""
                              --     Just bio ->
                              p [] [ text profile.bio ]
                            , if isViewingOwnProfile then
                                text ""

                              else
                                Utils.Maybe.view shared.user <|
                                    \user ->
                                        if profile.following then
                                            IconButton.view
                                                { color = IconButton.FilledGray
                                                , icon = IconButton.Plus
                                                , label = "Unfollow " ++ profile.username
                                                , onClick = ClickedUnfollow user profile
                                                }

                                        else
                                            IconButton.view
                                                { color = IconButton.OutlinedGray
                                                , icon = IconButton.Plus
                                                , label = "Follow " ++ profile.username
                                                , onClick = ClickedFollow user profile
                                                }
                            ]
                        ]
                    ]
                ]

        viewTabRow : Html Msg
        viewTabRow =
            div [ class "articles-toggle" ]
                [ ul [ class "nav nav-pills outline-active" ]
                    (List.map viewTab [ MyArticles, FavoritedArticles ])
                ]

        viewTab : Tab -> Html Msg
        viewTab tab =
            li [ class "nav-item" ]
                [ button
                    [ class "nav-link"
                    , Events.onClick (Clicked tab)
                    , classList [ ( "active", tab == model.selectedTab ) ]
                    ]
                    [ text
                        (case tab of
                            MyArticles ->
                                "My Articles"

                            FavoritedArticles ->
                                "Favorited Articles"
                        )
                    ]
                ]
    in
    div [ class "profile-page" ]
        [ viewUserInfo
        , div [ class "container" ]
            [ div [ class "row" ]
                [ div [ class "col-xs-12 col-md-10 offset-md-1" ]
                    (viewTabRow
                        :: Components.ArticleList.view
                            { user = shared.user
                            , articleListing = model.listing
                            , onFavorite = ClickedFavorite
                            , onUnfavorite = ClickedUnfavorite
                            , onPageClick = ClickedPage
                            }
                    )
                ]
            ]
        ]
