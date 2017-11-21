module Builder exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Test exposing (..)
import JsonSchema.Builder exposing (..)
import Json.Decode as Decode
import Result exposing (Result)


type alias Object =
    { a : String }


example =
    build
        (object Object
            |> with (field "a" string)
        )


testDecode : Test
testDecode =
    test "The example decodes." <|
        \_ ->
            case Decode.decodeString example.decoder "{ \"a\" : \"test\"}" of
                Ok object ->
                    Expect.equal object.a "test"

                Err error ->
                    Expect.fail <| "did not decode correctly." ++ error
