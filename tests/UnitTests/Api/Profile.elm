module UnitTests.Api.Profile exposing (suite)

import Conduit.Api.Profile
import Expect
import Json.Decode as Json
import Test exposing (..)


suite : Test
suite =
    describe "Conduit.Api.Profile"
        [ test "decodes example from spec" <|
            \_ ->
                """
                {
                    "username": "jake",
                    "bio": "I work at statefarm",
                    "image": "https://static.productionready.io/images/smiley-cyrus.jpg",
                    "following": false
                }
                """
                    |> Json.decodeString Conduit.Api.Profile.decoder
                    |> Expect.ok
        ]
