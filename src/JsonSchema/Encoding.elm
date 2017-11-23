module JsonSchema.Encoding exposing (Test)

{-| Very short docs.

@docs Test

-}

import Json.Encode as Encode exposing (Value)


{-| Just an experiment
-}
type alias Test =
    { a : Int
    , b : String
    }


test : Test
test =
    { a = 2, b = "tree" }


testEncoder : Test -> Value
testEncoder =
    (object Test
        |> with (field "a" .a integer)
        |> with (field "b" .b string)
    )
        |> build


build : (a -> List ( String, Value )) -> (a -> Value)
build fieldEncoder =
    fieldEncoder >> Encode.object


object _ =
    combineObjectEncoders


combineObjectEncoders :
    (obj -> List ( String, Value ))
    -> (obj -> List ( String, Value ))
    -> (obj -> List ( String, Value ))
combineObjectEncoders encode encodeRemainder obj =
    List.append (encode obj) (encodeRemainder obj)


with : a -> (a -> b) -> b
with a f =
    f a


field :
    String
    -> (obj -> field)
    -> (String -> field -> ( String, Value ))
    -> (obj -> List ( String, Value ))
field name extractor encoder =
    objectFieldEncoder extractor (encoder name)


objectFieldEncoder :
    (obj -> field)
    -> (field -> ( String, Value ))
    -> (obj -> List ( String, Value ))
objectFieldEncoder f encoder =
    f >> encoder >> List.singleton


integer : String -> Int -> ( String, Value )
integer name =
    encode name Encode.int


string : String -> String -> ( String, Value )
string name =
    encode name Encode.string


encode : String -> (a -> Value) -> a -> ( String, Value )
encode name encoder =
    (\field -> ( name, encoder field ))
