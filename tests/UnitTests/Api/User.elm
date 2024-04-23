module UnitTests.Api.User exposing (suite)

import Conduit.Api.User
import Expect
import Json.Decode as Json
import Test exposing (..)


suite : Test
suite =
    describe "Conduit.Api.User"
        [ test "decodes example from spec" <|
            \_ ->
                """
                {
                    "email": "jake@jake.jake",
                    "token": "jwt.token.here",
                    "username": "jake",
                    "bio": "I work at statefarm",
                    "image": null
                }
                """
                    |> Json.decodeString Conduit.Api.User.decoder
                    |> Expect.ok
        ]
