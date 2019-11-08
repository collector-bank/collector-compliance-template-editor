module SerializationTests exposing (suite)

import Fuzzers exposing (questionTemplateFuzzer, defaultOptions)
import Expect exposing (..)
import Test exposing (..)
import Fuzz exposing (..)
import JsonModel.Serialization exposing (toJson)
import JsonModel.Deserialization exposing (fromJson)

-- avoid empty ids for now since it will generate new ids which 
-- breaks the equality comparesion in the serialization / deserialization test
noEmptyIdsFuzzer : Fuzzer String
noEmptyIdsFuzzer = 
    oneOf [ constant "37c42e6e-7b58-44ae-a123-401a18b4fbbd", constant "e357e708-35eb-48e7-9fdc-afbeeea7807a" ]

suite : Test
suite =
    describe "Serialization and deserialization"
        [ fuzz (questionTemplateFuzzer { defaultOptions | idFuzzer = noEmptyIdsFuzzer }) "Serialization of any question template should be deserialized into an equivalent model" <| \qt ->
            let 
                serialized = toJson qt
                deserialized = fromJson serialized |> Result.toMaybe
            in          
                Expect.equal (Just qt) deserialized
        ]
