module JsonSchema.Encoding
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


obj :
    (obj -> List ( String, Value ))
    -> (obj -> List ( String, Value ))
    -> (obj -> List ( String, Value ))
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


objectSimpleFieldsEncoder =
    build
        (object ObjectSimpleFields
            |> with (field "a" .a string)
            |> with (field "b" .b integer)
         --|> with (field "c" .c number)
        )


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
object _ =
    combineObjectEncoders


combineObjectEncoders :
    (obj -> List ( String, Value ))
    -> (obj -> List ( String, Value ))
    -> (obj -> List ( String, Value ))
combineObjectEncoders encode encodeRemainder obj =
    List.append (encode obj) (encodeRemainder obj)


{-| Adds fields to an object.
-}
with : a -> (a -> b) -> b
with a f =
    f a


{-| Builds a field.
-}
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
