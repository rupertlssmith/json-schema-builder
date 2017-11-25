module JsonSchema.Encode
    exposing
        ( build
        , object
        , with
        , field
        , string
        , integer
        , number
        , boolean
        )

{-| Module docs

@docs string, integer, number, boolean
@docs build, object, with, field

-}

import Json.Encode as Encode exposing (Value)


type alias ObjectSimpleFields =
    { a : String
    , b : Int
    , c : Float
    , d : Bool
    }


type alias ObjectOuter =
    { inner : ObjectSimpleFields
    }


objectOuterEncoder =
    build
        (object ObjectOuter
            |> with
                (field "inner"
                    .inner
                    (object ObjectSimpleFields
                        |> with (field "a" .a string)
                        |> with (field "b" .b integer)
                        |> with (field "c" .c number)
                        |> with (field "d" .d boolean)
                    )
                )
        )


{-| Runs the builder.
-}
build : (obj -> List ( String, Value )) -> obj -> Value
build fieldEncoder =
    fieldEncoder >> Encode.object


{-| Builds an object.
-}
object : cons -> String -> obj -> List ( String, Value )
object _ obj =
    []


{-| Adds fields to an object.
-}
with : (obj -> List ( String, Value )) -> (obj -> List ( String, Value )) -> obj -> List ( String, Value )
with fieldEncoder remainderEncoder obj =
    List.append (fieldEncoder obj) (remainderEncoder obj)


{-| Builds a field.
-}
field :
    String
    -> (obj -> field)
    -> (String -> field -> List ( String, Value ))
    -> (obj -> List ( String, Value ))
field name lens encoder =
    lens >> (encoder name)


{-| Builds an integer type.
-}
integer : String -> Int -> List ( String, Value )
integer name =
    encode name Encode.int >> List.singleton


{-| Builds a string type.
-}
string : String -> String -> List ( String, Value )
string name =
    encode name Encode.string >> List.singleton


{-| Builds a number (float) type.
-}
number : String -> Float -> List ( String, Value )
number name =
    encode name Encode.float >> List.singleton


{-| Builds a boolean type.
-}
boolean : String -> Bool -> List ( String, Value )
boolean name =
    encode name Encode.bool >> List.singleton


encode : String -> (a -> Value) -> a -> ( String, Value )
encode name encoder =
    (\field -> ( name, encoder field ))
