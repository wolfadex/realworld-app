module Pages.Home_ exposing (Model, Msg, Tab, page)

import Api
import Api.Article.Tag exposing (Tag)
import Api.Data exposing (Data)
import Article
import Components.ArticleList
import Effect exposing (Effect)
import Html exposing (..)
import Html.Attributes exposing (class, classList)
import Html.Events as Events
import Http
import Layouts
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Utils.Maybe
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared _ =
    Page.new
        { init = init shared
        , update = update shared
        , subscriptions = subscriptions
        , view = view shared
        }
        |> Page.withLayout (\_ -> Layouts.Default {})



-- INIT


type alias Model =
    { listing : Data Article.Listing
    , page : Int
    , tags : Data (List Tag)
    , activeTab : Tab
    }


type Tab
    = FeedFor Api.User
    | Global
    | TagFilter Tag


init : Shared.Model -> () -> ( Model, Effect Msg )
init shared _ =
    let
        activeTab : Tab
        activeTab =
            shared.user
                |> Maybe.map FeedFor
                |> Maybe.withDefault Global

        model : Model
        model =
            { listing = Api.Data.Loading
            , page = 1
            , tags = Api.Data.Loading
            , activeTab = activeTab
            }
    in
    ( model
    , Effect.batch
        [ fetchArticlesForTab model
        , Api.Article.Tag.list { onResponse = GotTags }
            |> Effect.sendCmd
        ]
    )


fetchArticlesForTab :
    { model
        | page : Int
        , activeTab : Tab
    }
    -> Effect Msg
fetchArticlesForTab model =
    case model.activeTab of
        Global ->
            Api.getArticles
                { params =
                    { tag = Nothing
                    , author = Nothing
                    , favorited = Nothing
                    , offset = Just ((model.page - 1) * pageLimit)
                    , limit = Just pageLimit
                    }
                , toMsg = GotArticles model.page
                }
                |> Effect.sendCmd

        FeedFor user ->
            Api.getArticlesFeed
                { authorization = { token = user.token }
                , params =
                    { offset = Just ((model.page - 1) * pageLimit)
                    , limit = Just pageLimit
                    }
                , toMsg = GotArticles model.page
                }
                |> Effect.sendCmd

        TagFilter tag ->
            Api.getArticles
                { params =
                    { tag = Just tag
                    , author = Nothing
                    , favorited = Nothing
                    , offset = Just ((model.page - 1) * pageLimit)
                    , limit = Just pageLimit
                    }
                , toMsg = GotArticles model.page
                }
                |> Effect.sendCmd


pageLimit : Int
pageLimit =
    25



-- UPDATE


type Msg
    = GotArticles Int (Result Http.Error Api.MultipleArticlesResponse)
    | GotTags (Data (List Tag))
    | SelectedTab Tab
    | ClickedFavorite Api.User Api.Article
    | ClickedUnfavorite Api.User Api.Article
    | ClickedPage Int
    | UpdatedArticle (Result Http.Error Api.SingleArticleResponse)


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update _ msg model =
    case msg of
        GotArticles page_ response ->
            ( { model
                | listing =
                    response
                        |> Result.mapError (\_ -> [ "Articles error" ])
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

        GotTags tags ->
            ( { model | tags = tags }
            , Effect.none
            )

        SelectedTab tab ->
            let
                newModel : Model
                newModel =
                    { model
                        | activeTab = tab
                        , listing = Api.Data.Loading
                        , page = 1
                    }
            in
            ( newModel
            , fetchArticlesForTab newModel
            )

        ClickedFavorite user article ->
            ( model
            , Api.createArticleFavorite
                { authorization = { token = user.token }
                , params = { slug = article.slug }
                , toMsg = UpdatedArticle
                }
                |> Effect.sendCmd
            )

        ClickedUnfavorite user article ->
            ( model
            , Api.deleteArticleFavorite
                { authorization = { token = user.token }
                , params = { slug = article.slug }
                , toMsg = UpdatedArticle
                }
                |> Effect.sendCmd
            )

        ClickedPage page_ ->
            let
                newModel : Model
                newModel =
                    { model
                        | listing = Api.Data.Loading
                        , page = page_
                    }
            in
            ( newModel
            , fetchArticlesForTab newModel
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
    { title = ""
    , body =
        [ div [ class "home-page" ]
            [ div [ class "banner" ]
                [ div [ class "container" ]
                    [ h1 [ class "logo-font" ] [ text "conduit" ]
                    , p [] [ text "A place to share your knowledge." ]
                    ]
                ]
            , div [ class "container page" ]
                [ div [ class "row" ]
                    [ div [ class "col-md-9" ] <|
                        (viewTabs shared model
                            :: Components.ArticleList.view
                                { user = shared.user
                                , articleListing = model.listing
                                , onFavorite = ClickedFavorite
                                , onUnfavorite = ClickedUnfavorite
                                , onPageClick = ClickedPage
                                }
                        )
                    , div [ class "col-md-3" ] [ viewTags model.tags ]
                    ]
                ]
            ]
        ]
    }


viewTabs :
    Shared.Model
    -> { model | activeTab : Tab }
    -> Html Msg
viewTabs shared model =
    div [ class "feed-toggle" ]
        [ ul [ class "nav nav-pills outline-active" ]
            [ Utils.Maybe.view shared.user <|
                \user ->
                    li [ class "nav-item" ]
                        [ button
                            [ class "nav-link"
                            , classList [ ( "active", model.activeTab == FeedFor user ) ]
                            , Events.onClick (SelectedTab (FeedFor user))
                            ]
                            [ text "Your Feed" ]
                        ]
            , li [ class "nav-item" ]
                [ button
                    [ class "nav-link"
                    , classList [ ( "active", model.activeTab == Global ) ]
                    , Events.onClick (SelectedTab Global)
                    ]
                    [ text "Global Feed" ]
                ]
            , case model.activeTab of
                TagFilter tag ->
                    li [ class "nav-item" ] [ a [ class "nav-link active" ] [ text ("#" ++ tag) ] ]

                _ ->
                    text ""
            ]
        ]


viewTags : Data (List Tag) -> Html Msg
viewTags data =
    case data of
        Api.Data.Success tags ->
            div [ class "sidebar" ]
                [ p [] [ text "Popular Tags" ]
                , div [ class "tag-list" ] <|
                    List.map
                        (\tag ->
                            button
                                [ class "tag-pill tag-default"
                                , Events.onClick (SelectedTab (TagFilter tag))
                                ]
                                [ text tag ]
                        )
                        tags
                ]

        _ ->
            text ""
