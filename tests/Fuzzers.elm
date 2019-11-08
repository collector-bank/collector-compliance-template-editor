module Fuzzers exposing (FuzzerOptions, defaultOptions, questionTemplateFuzzer)

import Fuzz exposing (Fuzzer, int, list, string, andMap, map, map2, constant, oneOf, bool)

import QuestionTemplate.Model exposing (..)
import QuestionCategory.Model exposing (..)
import Question.Model exposing (..)

type alias FuzzerOptions = 
    { idFuzzer : Fuzzer String
    , maxDepth : Int
    }

defaultOptions : FuzzerOptions
defaultOptions = 
    { idFuzzer = defaultIdFuzzer
    , maxDepth = 2 }

defaultIdFuzzer : Fuzzer String
defaultIdFuzzer = 
    oneOf [ constant "37c42e6e-7b58-44ae-a123-401a18b4fbbd", constant "e357e708-35eb-48e7-9fdc-afbeeea7807a", constant ""]

-- faster (but dumb) string generator
string_ : Fuzzer String
string_ = 
    Fuzz.frequency 
        [ (1, constant "")
        , (10, constant "some normal string")
        , (1, constant "some long string some long stringsome long stringsome long stringsome long stringsome long stringsome long stringsome long stringsome long stringsome long stringsome long stringsome long string")
        , (1, constant "åäö$%)[]){}")
        ]

optionFuzzer : FuzzerOptions -> Fuzzer QuestionOption
optionFuzzer options =
    (constant QuestionOption)
    |> andMap options.idFuzzer -- id
    |> andMap string_ -- text
    |> andMap bool -- isComplianceCheckNeeded
    |> andMap (questionsFuzzer options)  -- followUpQuestions
    |> andMap (constant False) -- active (must be default value which is false since this will not be serialized)

questionTypeFuzzer : FuzzerOptions -> Fuzzer QuestionType
questionTypeFuzzer options =
    let
        freeTextQuestionFuzzer = 
            (constant FreeTextQuestionFields) 
            |> andMap (map String.fromInt int) -- maxLength
            |> andMap bool   -- isMandatory
            |> map (\fields -> FreeTextQuestion fields)
        selectQuestionFuzzer depth = 
            (constant SelectQuestionFields)
            |> andMap (list (optionFuzzer options)) -- options
            |> andMap bool -- allowMultiple
            |> map (\fields -> SelectQuestion fields)
        countryQuestionFuzzer =
            (constant CountryQuestionFields)
            |> andMap bool -- allowMultiple
            |> andMap string_ -- validationMessage
            |> map (\fields -> CountryQuestion fields)
        questionGroupFuzzer depth =
            (constant QuestionGroupFields)
            |> andMap (questionsFuzzer depth) -- questions
            |> andMap (map String.fromInt int) -- maxRepeat
            |> map (\fields -> QuestionGroup fields)
    in
        oneOf 
            [ freeTextQuestionFuzzer
            , selectQuestionFuzzer options
            , countryQuestionFuzzer
            , questionGroupFuzzer options
            ]

-- avoid parameter list with more than one parameter for now
-- since the serialization order is not deterministic
parametersFuzzer : Fuzzer (List (String,String))
parametersFuzzer =
    oneOf 
        [ constant []
        , map2 (\a b -> [(a,b)]) string_ string_
        ]

questionFuzzer : FuzzerOptions -> Fuzzer Question
questionFuzzer options =
    (constant QuestionFields)
    |> andMap options.idFuzzer  --id
    |> andMap string_ -- title
    |> andMap string_ -- extendedTitle
    |> andMap string_ -- description
    |> andMap string_ -- extendedDescription
    |> andMap parametersFuzzer -- parameters
    |> andMap (questionTypeFuzzer options)
    |> andMap (constant False) -- active (must be default value which is false since this will not be serialized)
    |> andMap (constant False) -- collapsed (must be default value which is false since this will not be serialized)
    |> map (\fields -> Question fields)

questionsFuzzer : FuzzerOptions -> Fuzzer (List Question)
questionsFuzzer options  = 
    if options.maxDepth <= 0 then
        Fuzz.constant []
    else
        let 
            qf = questionFuzzer {options | maxDepth = options.maxDepth-1 }
        in 
            -- avoid using standard list fuzzer since it generates very long lists sometimes which is too expensive
            Fuzz.frequency 
                [ (1.0, constant [])
                , (3.0, Fuzz.map (\a -> [a]) qf)
                , (3.0, Fuzz.map2 (\a b -> [a, b]) qf qf)
                , (2.0, Fuzz.map3 (\a b c -> [a, b, c]) qf qf qf)
                ]

questionCategoryFuzzer : FuzzerOptions -> Fuzzer QuestionCategory
questionCategoryFuzzer options =
    (constant QuestionCategory)
    |> andMap string_  -- title
    |> andMap string_  -- extended title
    |> andMap string_  -- description
    |> andMap string_  -- extendedDescription
    |> andMap (oneOf [ constant AML, constant PEP ]) -- categoryType
    |> andMap (questionsFuzzer options)  -- questions
    |> andMap (constant False)  -- active (must be default value which is false since this will not be serialized)

questionTemplateFuzzer : FuzzerOptions -> Fuzzer QuestionTemplate
questionTemplateFuzzer options = 
    map2 (\name categories -> { name = name, categories = categories })
        string
        (list <| questionCategoryFuzzer options)
