module JsonSchema.Builder
    exposing
        ( build
        , object
        , with
        , field
        , string
        , integer
        , number
        , boolean
        , Object(..)
        , Field(..)
        )

{-| Module docs

@docs Object, Field
@docs string, integer, number, boolean
@docs build, object, with, field

-}


type alias ObjectSimpleFields =
    { a : String
    , b : Int
    , c : Float
    , d : Bool
    }


objectSimpleFieldsSpec =
    object ObjectSimpleFields
        |> with (field "a" .a string)
        |> with (field "b" .b integer)
        |> with (field "c" .c number)
        |> with (field "d" .d boolean)
        |> build


{-| Object specifications.
-}
type Object
    = Nah



--
--
--type Field obj
--    = IntField String (obj -> Int)
--    | StrField String (obj -> String)
--    | NumField String (obj -> Float)
--    | BoolField String (obj -> Bool)


{-| Field specifications.
-}
type Field
    = IntField
    | StrField
    | NumField
    | BoolField
    | Object (List ( String, Field ))


{-| Runs the builder.
-}
build : (obj -> List ( String, Field )) -> obj -> Field
build fieldEncoder =
    fieldEncoder >> encodeObject


{-| Builds an object.
-}
object : cons -> obj -> List ( String, Field )
object _ obj =
    []


{-| Adds fields to an object.
-}
with : (obj -> List ( String, Field )) -> (obj -> List ( String, Field )) -> obj -> List ( String, Field )
with fieldEncoder remainderEncoder obj =
    List.append (fieldEncoder obj) (remainderEncoder obj)


{-| Builds a field.
-}
field :
    String
    -> (obj -> field)
    -> (field -> Field)
    -> (obj -> List ( String, Field ))
field name lens encoder =
    lens >> (encode name encoder >> List.singleton)


{-| Builds an integer type.
-}
integer : Int -> Field
integer =
    encodeInt


{-| Builds a string type.
-}
string : String -> Field
string =
    encodeString


{-| Builds a number (float) type.
-}
number : Float -> Field
number =
    encodeFloat


{-| Builds a boolean type.
-}
boolean : Bool -> Field
boolean =
    encodeBool


encode : String -> (a -> Field) -> a -> ( String, Field )
encode name encoder =
    (\field -> ( name, encoder field ))


encodeInt : Int -> Field
encodeInt _ =
    IntField


encodeString : String -> Field
encodeString _ =
    StrField


encodeFloat : Float -> Field
encodeFloat _ =
    NumField


encodeBool : Bool -> Field
encodeBool _ =
    BoolField


encodeObject : List ( String, Field ) -> Field
encodeObject fields =
    Object fields
