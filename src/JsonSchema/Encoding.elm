module JsonSchema.Encoding exposing (Test)

{-| Very short docs.

@docs Test

-}

import Json.Encode as Encode


{-| Just an experiment
-}
type alias Test =
    { a : Int
    , b : String
    }


test : Test
test =
    { a = 2, b = "tree" }


f : Int -> List ( String, Encode.Value )
f a =
    [ (\field -> ( "a", Encode.int field )) a ]


g : String -> List ( String, Encode.Value )
g b =
    [ (\field -> ( "b", Encode.string field )) b ]


h : Int -> String -> List ( String, Encode.Value )
h =
    combine f g


encodedFields : Test -> Encode.Value
encodedFields test =
    h test.a test.b
        |> Encode.object


combine :
    (a -> List ( String, Encode.Value ))
    -> (b -> List ( String, Encode.Value ))
    -> (a -> b -> List ( String, Encode.Value ))
combine encodeA encodeB a b =
    List.append (encodeA a) (encodeB b)


testEncoder : Test -> Encode.Value
testEncoder model =
    [ (\field -> ( "a", Encode.int field )) model.a
    , (\field -> ( "b", Encode.string field )) model.b
    ]
        |> Encode.object
