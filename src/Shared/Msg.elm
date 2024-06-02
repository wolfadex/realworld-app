module Shared.Msg exposing (Msg(..))

import Conduit.Types


type Msg
    = ClickedSignOut
    | SignedInUser Conduit.Types.User
