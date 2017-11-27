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
type Field obj
    = IntField (obj -> Int)
    | StrField (obj -> String)
    | NumField (obj -> Float)
    | BoolField (obj -> Bool)
    | Object (List ( String, Field obj ))


{-| Runs the builder.
-}
build : (obj -> List ( String, Field obj )) -> obj -> Field obj
build fieldEncoder =
    fieldEncoder >> encodeObject


encodeObject : List ( String, Field obj ) -> Field obj
encodeObject fields =
    Object fields


{-| Builds an object.
-}
object : cons -> obj -> List ( String, Field obj )
object _ obj =
    []


{-| Adds fields to an object.
-}
with : (obj -> List ( String, Field obj )) -> (obj -> List ( String, Field obj )) -> obj -> List ( String, Field obj )
with fieldEncoder remainderEncoder obj =
    List.append (fieldEncoder obj) (remainderEncoder obj)


{-| Builds a field.
-}
field :
    String
    -> (obj -> field)
    -> ((obj -> field) -> field -> Field obj)
    -> (obj -> List ( String, Field obj ))
field name lens encoder =
    lens >> (encode name lens encoder >> List.singleton)


encode : String -> (obj -> field) -> ((obj -> field) -> field -> Field obj) -> field -> ( String, Field obj )
encode name lens encoder =
    (\field -> ( name, encoder lens field ))


{-| Builds an integer type.
-}
integer : (obj -> Int) -> Int -> Field obj
integer lens =
    encodeInt lens


{-| Builds a string type.
-}
string : (obj -> String) -> String -> Field obj
string lens =
    encodeString lens


{-| Builds a number (float) type.
-}
number : (obj -> Float) -> Float -> Field obj
number lens =
    encodeFloat lens


{-| Builds a boolean type.
-}
boolean : (obj -> Bool) -> Bool -> Field obj
boolean lens =
    encodeBool lens


encodeInt lens _ =
    IntField lens


encodeString lens _ =
    StrField lens


encodeFloat lens _ =
    NumField lens


encodeBool lens _ =
    BoolField lens
