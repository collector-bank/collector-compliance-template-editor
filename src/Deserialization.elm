module Deserialization exposing (parseProducts)

import Json.Decode as D exposing (..)
import Json.Decode.Pipeline as DP exposing (..)
import Model exposing (ProductDefinition)

parseProducts : String -> Result Error (List ProductDefinition)
parseProducts = decodeString (D.list decodeProduct)

decodeProduct : Decoder ProductDefinition
decodeProduct = 
    succeed ProductDefinition
        |> DP.required "id" string
        |> DP.required "name" string
