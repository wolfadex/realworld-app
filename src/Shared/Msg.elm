module Shared.Msg exposing (Msg(..))

import Conduit.Api


type Msg
    = ClickedSignOut
    | SignedInUser Conduit.Api.User
