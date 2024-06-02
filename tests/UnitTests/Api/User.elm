module UnitTests.Api.User exposing (suite)

import Expect
import Json.Decode as Json
import OpenApi.Common
import Test exposing (..)


suite : Test
suite =
    describe "OpenApi.Common"
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
                    |> Json.decodeString OpenApi.Common.decoder
                    |> Expect.ok
        ]
