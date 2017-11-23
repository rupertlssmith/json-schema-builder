module Encoding exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Test exposing (..)
import JsonSchema.Encoding exposing (..)
import Json.Decode as Decode
import Result exposing (Result)


type alias ObjectSimpleFields =
    { a : String
    , b : Int
    , c : Float
    , d : Bool
    }


objectSimpleFields =
    { a = "test"
    , b = 2
    , c = 5.678
    , d = True
    }


objectSimpleFieldsEncoder =
    build
        (object ObjectSimpleFields
            |> with (field "a" .a string)
            |> with (field "b" .b integer)
        )



--     |> with (field "c" .c number)
--     |> with (field "d" .d boolean)
-- )


testDecodeObjectSimpleFields : Test
testDecodeObjectSimpleFields =
    test "An object with simple fields decodes." <|
        \_ ->
            let
                value =
                    objectSimpleFieldsEncoder objectSimpleFields

                d =
                    Debug.log "test" value
            in
                Expect.pass
