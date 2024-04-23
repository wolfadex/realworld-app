module Auth exposing (User, onPageLoad, viewCustomPage, viewLoadingPage)

import Auth.Action
import Conduit.Api
import Dict
import Route exposing (Route)
import Route.Path
import Shared
import View exposing (View)


type alias User =
    Conduit.Api.User


onPageLoad : Shared.Model -> Route () -> Auth.Action.Action User
onPageLoad shared _ =
    case shared.user of
        Just user ->
            Auth.Action.loadPageWithUser user

        Nothing ->
            Auth.Action.replaceRoute
                { path = Route.Path.Login
                , query = Dict.empty
                , hash = Nothing
                }


viewLoadingPage : Shared.Model -> Route () -> View Never
viewLoadingPage _ _ =
    View.none


viewCustomPage : Shared.Model -> Route () -> View Never
viewCustomPage shared route =
    View.none
