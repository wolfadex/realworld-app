module Shared.Model exposing (Model)

import Conduit.Api


type alias Model =
    { user : Maybe Conduit.Api.User
    }
