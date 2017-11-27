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


simpleEncoder =
    build
        (object ObjectSimpleFields
            |> with (field "a" .a string)
            |> with (field "b" .b integer)
            |> with (field "c" .c number)
            |> with (field "d" .d boolean)
        )


{-| Runs the builder.
-}
build : (obj -> List ( String, Value )) -> obj -> Value
build fieldEncoder =
    fieldEncoder >> Encode.object


{-| Builds an object.
-}
object : cons -> obj -> List ( String, Value )
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
    -> (field -> Value)
    -> (obj -> List ( String, Value ))
field name lens encoder =
    lens >> (encode name encoder >> List.singleton)


{-| Builds an integer type.
-}
integer : Int -> Value
integer =
    Encode.int


{-| Builds a string type.
-}
string : String -> Value
string =
    Encode.string


{-| Builds a number (float) type.
-}
number : Float -> Value
number =
    Encode.float


{-| Builds a boolean type.
-}
boolean : Bool -> Value
boolean =
    Encode.bool


encode : String -> (a -> Value) -> a -> ( String, Value )
encode name encoder =
    (\field -> ( name, encoder field ))
