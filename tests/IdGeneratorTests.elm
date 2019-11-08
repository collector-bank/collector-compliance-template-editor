module IdGeneratorTests exposing (..)

import Fuzzers exposing (questionTemplateFuzzer, defaultOptions)
import Test exposing (..)
import Expect exposing (true)
import Fuzz exposing (..)
import List exposing (all)

import QuestionTemplate.Model exposing (..)
import QuestionCategory.Model exposing (..)
import Question.Model exposing (..)
import JsonModel.IdGenerator exposing (generateIds)

questionHasIds : Question -> Bool
questionHasIds (Question q) = q.id /= ""  -- todo check recursively

questionCategoriesHasIds : QuestionCategory -> Bool
questionCategoriesHasIds qc = all questionHasIds qc.questions

questionTemplateHasIds : QuestionTemplate -> Bool
questionTemplateHasIds qt = all questionCategoriesHasIds qt.categories

onlyEmptyIdsFuzzer : Fuzzer String
onlyEmptyIdsFuzzer = constant ""

suite : Test
suite =
    describe "Id generation"
        [ fuzz (questionTemplateFuzzer ({ defaultOptions | idFuzzer = onlyEmptyIdsFuzzer, maxDepth = 1 })) "Every id taggable object should have an id after generation" <| \qt ->
            let 
                qt_ = generateIds qt
            in          
                Expect.true "Should have ids" (questionTemplateHasIds qt_)
        ]
