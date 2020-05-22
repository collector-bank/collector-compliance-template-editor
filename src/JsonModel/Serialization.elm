module JsonModel.Serialization exposing (toJson)

import QuestionTemplate.Model exposing (..)
import QuestionCategory.Model exposing (..)
import Question.Model exposing (..)
import Json.Encode exposing (..)

-- Utilities

emptyPropertySet : List (String, Value)
emptyPropertySet = []

addProperty : String -> (a -> Value) -> a -> List (String, Value) -> List (String, Value)
addProperty name jsonifier value properties = properties ++ [(name,jsonifier value)]

addOptionalProperty : String -> (a -> Maybe Value) -> a -> List (String, Value) -> List (String, Value)
addOptionalProperty name jsonifier value properties = 
    case jsonifier value of
        Just value_ -> properties ++ [(name,value_)]
        Nothing -> properties

addProperties : List (String, Value) -> List (String, Value) -> List (String, Value)
addProperties a b = a ++ b

stringToOptionalInt : String -> Maybe Value
stringToOptionalInt s = s |> String.toInt |> Maybe.map Json.Encode.int

nonEmptyStringOrDefault : String -> Maybe Value
nonEmptyStringOrDefault v = if v == "" then Nothing else Just (string v)

nonEmptyListOrDefault : (List a -> Value) -> List a -> Maybe Value
nonEmptyListOrDefault f v = if List.isEmpty v then Nothing else Just (f v)

defaultOrValue : (a -> Value) -> a -> a -> Maybe Value
defaultOrValue f dv v = if v == dv then Nothing else Just (f v)

-- Serialization

toJson : QuestionTemplate -> String
toJson = questionTemplateToJson >> encode 4

questionTemplateToJson : QuestionTemplate -> Value
questionTemplateToJson questionTemplate =
    object 
        [ ("name", string questionTemplate.name)
        , ("categories", questionCategoriesToJson questionTemplate.categories )
        ]

questionCategoriesToJson : List QuestionCategory -> Value
questionCategoriesToJson questionCategories = list questionCategoryToJson questionCategories 

questionCategoryToJson : QuestionCategory -> Value
questionCategoryToJson questionCategory =
    let 
        categoryTypeToValue ct = int <|
            case ct of 
                AML -> 1
                PEP -> 0 
    in            
        object <|
            (
                emptyPropertySet |>
                addProperty "title" string questionCategory.title |>
                addProperty "categoryType" categoryTypeToValue questionCategory.categoryType |>
                addOptionalProperty "description" nonEmptyStringOrDefault questionCategory.description |>
                addOptionalProperty "extendedDescription" nonEmptyStringOrDefault questionCategory.extendedDescription |>
                addOptionalProperty "extendedTitle" nonEmptyStringOrDefault questionCategory.extendedTitle |>
                addProperty "questions" questionsToJson questionCategory.questions
            )

questionsToJson : List Question -> Value
questionsToJson questions = list questionToJson questions

questionToJson : Question -> Value
questionToJson (Question question) = 
    let
        toSelectType b = if b then int 1 else int 0
        questionTypeSpecifics =             
            case question.questionType of
                FreeTextQuestion ftq -> 
                    emptyPropertySet |>                
                    addProperty "questionType" int 1 |>
                    addOptionalProperty "maxLength" stringToOptionalInt ftq.maxLength |>
                    addOptionalProperty "isMandatory" (defaultOrValue bool False) ftq.isMandatory
                SelectQuestion sq -> 
                    emptyPropertySet |>
                    addProperty "questionType" int 0 |>
                    addProperty "selectType" toSelectType sq.allowMultiple |>
                    addProperty "options" optionsToJson sq.options
                CountryQuestion cq -> 
                    emptyPropertySet |>
                    addProperty "questionType" int 2 |>
                    addProperty "countryListFor" int 0 |>
                    addProperty "selectType" toSelectType cq.allowMultiple |>
                    addOptionalProperty "validationMessage" nonEmptyStringOrDefault cq.validationMessage
                QuestionGroup qg -> 
                    emptyPropertySet |>
                    addProperty "questionType" int 3 |>
                    addProperty "questions" questionsToJson qg.questions |>
                    addOptionalProperty "maxRepeat" stringToOptionalInt qg.maxRepeat
                BeneficialOwnersQuestion ->
                    emptyPropertySet |>
                    addProperty "questionType" int 4
    in        
        object <| 
            (
                emptyPropertySet |>
                addProperty "id" string question.id |>
                addProperty "title" string question.title |>
                addOptionalProperty "extendedTitle" nonEmptyStringOrDefault question.extendedTitle |>
                addOptionalProperty "description" nonEmptyStringOrDefault question.description |>
                addOptionalProperty "extendedDescription" nonEmptyStringOrDefault question.extendedDescription |>
                addOptionalProperty "parameters" (nonEmptyListOrDefault parametersToJson) question.parameters |>
                addProperties questionTypeSpecifics
            )

optionsToJson : List QuestionOption -> Value
optionsToJson options = list optionToJson options

optionToJson : QuestionOption -> Value
optionToJson option = 
    object <|
        (
            emptyPropertySet |>
            addProperty "id" string option.id |>
            addProperty "text" string option.text |>
            addOptionalProperty "isComplianceCheckNeeded" (defaultOrValue bool False) option.isComplianceCheckNeeded |>
            addProperty "followUpQuestions" questionsToJson option.followUpQuestions
        )

parametersToJson : List (String,String) -> Value
parametersToJson parameters = parameters |> List.map (\(name,value) -> (name, string value)) |> object
