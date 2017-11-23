module Encoding exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Test exposing (..)
import JsonSchema.Encoding exposing (..)
import Json.Encode exposing (Value)
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


obj : (obj -> List ( String, Value )) -> (obj -> List ( String, Value )) -> (obj -> List ( String, Value ))
obj =
    (object ObjectSimpleFields)


one : ({ a | a : String } -> List ( String, Value )) -> { a | a : String } -> List ( String, Value )
one =
    (object ObjectSimpleFields)
        |> with (field "a" .a string)


two : { a | a : String, b : Int } -> List ( String, Value )
two =
    ((object ObjectSimpleFields)
        |> with (field "a" .a string)
    )
        |> with (field "b" .b integer)


strf : (({ a | a : String } -> List ( String, Value )) -> b) -> b
strf =
    with (field "a" .a string)


intf : (({ a | b : Int } -> List ( String, Value )) -> b) -> b
intf =
    with (field "b" .b integer)


numf : (({ a | c : Float } -> List ( String, Value )) -> b) -> b
numf =
    with (field "c" .c number)
