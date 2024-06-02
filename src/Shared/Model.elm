module Shared.Model exposing (Model)

import Conduit.Types


type alias Model =
    { user : Maybe Conduit.Types.User
    }
