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


encode : String -> (a -> Value) -> a -> ( String, Value )
encode name encoder =
    (\field -> ( name, encoder field ))


integer : String -> Int -> ( String, Value )
integer name =
    encode name Encode.int


string : String -> String -> ( String, Value )
string name =
    encode name Encode.string


compose :
    (obj -> field)
    -> (field -> ( String, Encode.Value ))
    -> (obj -> ( String, Encode.Value ))
compose f encoder =
    f >> encoder


wrap :
    (obj -> ( String, Encode.Value ))
    -> (obj -> List ( String, Encode.Value ))
wrap encode obj =
    [ encode obj ]


combine :
    (obj -> ( String, Encode.Value ))
    -> (obj -> List ( String, Encode.Value ))
    -> (obj -> List ( String, Encode.Value ))
combine encode encodeRemainder obj =
    (encode obj) :: (encodeRemainder obj)


encodeTest : Test -> Value
encodeTest test =
    Encode.object
        (combine
            (compose .a (integer "a"))
            (wrap (compose .b (string "b")))
            test
        )



-- field =
--     compose
--
--
-- object _ =
--     combine
--
--
-- with =
--     identity
--
--
-- build =
--     Encode.object
--
--
-- encodeTest2 : Test -> Value
-- encodeTest2 test =
--     build
--         (object Test
--             (with (field .a (integer "a")))
--             (with (wrap (field .b (string "b"))))
--             test
--         )
