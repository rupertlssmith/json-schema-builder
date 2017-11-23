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


{-| Runs the builder.
-}
build : (a -> List ( String, Value )) -> (a -> Value)
build fieldEncoder =
    fieldEncoder >> Encode.object


{-| Builds an object.
-}
object :
    cons
    -> (obj -> List ( String, Value ))
    -> (obj -> List ( String, Value ))
    -> (obj -> List ( String, Value ))
object _ encodeField encodeRemainder obj =
    List.append (encodeField obj) (encodeRemainder obj)


{-| Adds fields to an object.
-}
with : a -> (a -> b) -> b
with field object =
    object field


{-| Builds a field.
-}
field :
    String
    -> (obj -> field)
    -> (String -> field -> ( String, Value ))
    -> (obj -> List ( String, Value ))
field name extractor encoder =
    extractor >> (encoder name) >> List.singleton


{-| Builds an integer type.
-}
integer : String -> Int -> ( String, Value )
integer name =
    encode name Encode.int


{-| Builds a string type.
-}
string : String -> String -> ( String, Value )
string name =
    encode name Encode.string


{-| Builds a number (float) type.
-}
number : String -> Float -> ( String, Value )
number name =
    encode name Encode.float


{-| Builds a boolean type.
-}
boolean : String -> Bool -> ( String, Value )
boolean name =
    encode name Encode.bool


encode : String -> (a -> Value) -> a -> ( String, Value )
encode name encoder =
    (\field -> ( name, encoder field ))