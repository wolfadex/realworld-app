port module Effect exposing
    ( Effect
    , none, batch
    , sendCmd
    , pushRoute, replaceRoute
    , map, toCmd
    , signIn, signOut
    , saveUser, clearUser
    )

{-|

@docs Effect
@docs none, batch
@docs sendCmd
@docs pushRoute, replaceRoute

@docs map, toCmd


## Custom

@docs signIn, signOut
@docs saveUser, clearUser

-}

import Browser.Navigation
import Conduit.Json
import Conduit.Types
import Dict exposing (Dict)
import Json.Decode as Json
import Json.Encode as Encode
import Route
import Route.Path
import Shared.Model
import Shared.Msg
import Task
import Url exposing (Url)


port outgoing :
    { tag : String
    , data : Json.Value
    }
    -> Cmd msg


type Effect msg
    = -- BASICS
      None
    | Batch (List (Effect msg))
    | SendCmd (Cmd msg)
      -- ROUTING
    | PushUrl String
    | ReplaceUrl String
      -- SHARED
    | SendSharedMsg Shared.Msg.Msg
      -- USERS
    | SaveUser Conduit.Types.User
    | ClearUser



-- BASICS


{-| Don't send any effect.
-}
none : Effect msg
none =
    None


{-| Send multiple effects at once.
-}
batch : List (Effect msg) -> Effect msg
batch =
    Batch


{-| Send a normal `Cmd msg` as an effect, something like `Http.get` or `Random.generate`.
-}
sendCmd : Cmd msg -> Effect msg
sendCmd =
    SendCmd



-- ROUTING


{-| Set the new route, and make the back button go back to the current route.
-}
pushRoute :
    { path : Route.Path.Path
    , query : Dict String String
    , hash : Maybe String
    }
    -> Effect msg
pushRoute route =
    PushUrl (Route.toString route)


{-| Set the new route, but replace the previous one, so clicking the back
button **won't** go back to the previous route.
-}
replaceRoute :
    { path : Route.Path.Path
    , query : Dict String String
    , hash : Maybe String
    }
    -> Effect msg
replaceRoute route =
    ReplaceUrl (Route.toString route)



-- USERS


signIn : Conduit.Types.User -> Effect msg
signIn user =
    SendSharedMsg (Shared.Msg.SignedInUser user)


signOut : Effect msg
signOut =
    SendSharedMsg Shared.Msg.ClickedSignOut


saveUser : Conduit.Types.User -> Effect msg
saveUser user =
    SaveUser user


clearUser : Effect msg
clearUser =
    ClearUser



-- INTERNALS


{-| Elm Land depends on this function to connect pages and layouts
together into the overall app.
-}
map : (msg1 -> msg2) -> Effect msg1 -> Effect msg2
map fn effect =
    case effect of
        None ->
            None

        Batch list ->
            Batch (List.map (map fn) list)

        SendCmd cmd ->
            SendCmd (Cmd.map fn cmd)

        PushUrl url ->
            PushUrl url

        ReplaceUrl url ->
            ReplaceUrl url

        SendSharedMsg sharedMsg ->
            SendSharedMsg sharedMsg

        SaveUser user ->
            SaveUser user

        ClearUser ->
            ClearUser


{-| Elm Land depends on this function to perform your effects.
-}
toCmd :
    { key : Browser.Navigation.Key
    , url : Url
    , shared : Shared.Model.Model
    , fromSharedMsg : Shared.Msg.Msg -> msg
    , batch : List msg -> msg
    , toCmd : msg -> Cmd msg
    }
    -> Effect msg
    -> Cmd msg
toCmd options effect =
    case effect of
        None ->
            Cmd.none

        Batch list ->
            Cmd.batch (List.map (toCmd options) list)

        SendCmd cmd ->
            cmd

        PushUrl url ->
            Browser.Navigation.pushUrl options.key url

        ReplaceUrl url ->
            Browser.Navigation.replaceUrl options.key url

        SendSharedMsg sharedMsg ->
            Task.succeed sharedMsg
                |> Task.perform options.fromSharedMsg

        SaveUser user ->
            outgoing
                { tag = "saveUser"
                , data = Conduit.Json.encodeUser user
                }

        ClearUser ->
            outgoing
                { tag = "clearUser"
                , data = Encode.null
                }
