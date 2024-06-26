module Shared exposing
    ( Flags
    , Model
    , Msg
    , decoder
    , init
    , subscriptions
    , update
    )

import Conduit.Json
import Conduit.Types
import Effect exposing (Effect)
import Json.Decode
import Route exposing (Route)
import Shared.Model
import Shared.Msg



-- INIT


type alias Flags =
    { user : Maybe Conduit.Types.User
    }


decoder : Json.Decode.Decoder Flags
decoder =
    Json.Decode.map Flags
        (Json.Decode.maybe (Json.Decode.field "user" Conduit.Json.decodeUser))


type alias Model =
    Shared.Model.Model


init : Result Json.Decode.Error Flags -> Route () -> ( Model, Effect Msg )
init result _ =
    let
        flags =
            result
                |> Result.withDefault
                    { user = Nothing
                    }
    in
    ( { user = flags.user
      }
    , Effect.none
    )



-- UPDATE


type alias Msg =
    Shared.Msg.Msg


update : Route () -> Msg -> Model -> ( Model, Effect Msg )
update _ msg model =
    case msg of
        Shared.Msg.SignedInUser user ->
            ( { model | user = Just user }
            , Effect.saveUser user
            )

        Shared.Msg.ClickedSignOut ->
            ( { model | user = Nothing }
            , Effect.clearUser
            )


subscriptions : Route () -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none



-- VIEW
