module JsonModel.Deserialization exposing (fromJson)

import QuestionCategory.Model exposing(..)
import Question.Model exposing (..)
import QuestionTemplate.Model exposing (..)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import JsonModel.IdGenerator exposing (generateIds)                        

fromJson : String -> Result Error QuestionTemplate
fromJson s = decodeString decodeQuestionTemplate s |> Result.map generateIds

decodeQuestionTemplate : Decoder QuestionTemplate
decodeQuestionTemplate = 
    succeed QuestionTemplate 
        |> required "name" string
        |> required "categories" decodeQuestionCategories

decodeQuestionCategories : Decoder (List QuestionCategory)
decodeQuestionCategories = list decodeQuestionCategory

decodeQuestionCategory : Decoder QuestionCategory
decodeQuestionCategory = 
    let
        makeQuestionCategory title extendedTitle description extendedDescription categoryType questions active =
            let 
                makeResult ct = succeed { title=title, extendedTitle=extendedTitle, description=description, extendedDescription=extendedDescription, categoryType=ct, questions=questions, active=active }
            in
                case categoryType of
                    1 -> makeResult AML
                    0 -> makeResult PEP
                    _ -> fail "Unknown category type."
    in        
        succeed makeQuestionCategory
            |> optional "title" string ""
            |> optional "extendedTitle" string ""
            |> optional "description" string ""
            |> optional "extendedDescription" string ""
            |> required "categoryType" int 
            |> optional "questions" decodeQuestions []        
            |> hardcoded False -- active
            |> resolve 

decodeQuestions : Decoder (List Question)
decodeQuestions = list (lazy (\_ -> decodeQuestion))

decodeQuestion : Decoder Question 
decodeQuestion = 
    let
        decodeQuestionRecord =
            let
                toDecoder title extendedTitle description extendedDescription questionTypeValue active id collapsed maybeMaxLength options allowMultiple questions parameters maybeMaxRepeat validationMessage isMandatory =
                    let 
                        questionType =
                            case questionTypeValue of
                                1 -> Just <|
                                    case maybeMaxLength of
                                        Just maxLength  -> FreeTextQuestion { maxLength = String.fromInt maxLength, isMandatory = isMandatory }
                                        Nothing -> makeFreeTextQuestion
                                0 -> Just <| SelectQuestion { options = options, allowMultiple = allowMultiple }      
                                2 -> Just <| CountryQuestion { allowMultiple = allowMultiple, validationMessage = validationMessage } 
                                3 -> Just <| 
                                    case maybeMaxRepeat of
                                        Just maxRepeat -> QuestionGroup { questions = questions, maxRepeat = String.fromInt maxRepeat }
                                        Nothing -> makeQuestionGroup
                                4 -> Just <| BeneficialOwnersQuestion
                                _ -> Nothing              
                    in
                        case questionType of
                            Just qt -> succeed 
                                { id = id
                                , title = title
                                , extendedTitle = extendedTitle
                                , description = description
                                , extendedDescription =  extendedDescription
                                , parameters = parameters
                                , questionType = qt
                                , active = False, collapsed = False }
                            Nothing -> fail "Failed to parse question fields"                
            in                
                succeed toDecoder
                    |> required "title" string
                    |> optional "extendedTitle" string ""
                    |> optional "description" string ""
                    |> optional "extendedDescription" string ""
                    |> required "questionType" int
                    |> hardcoded False -- active
                    |> optional "id" string ""
                    |> hardcoded False -- collapsed
                    |> optional "maxLength" (map Just int) Nothing
                    |> optional "options" (lazy (\_ -> decodeOptions)) []
                    |> optional "selectType" decodeSelectType False
                    |> optional "questions" (lazy (\_ -> decodeQuestions)) []
                    |> optional "parameters" (keyValuePairs string) []
                    |> optional "maxRepeat" (map Just int) Nothing
                    |> optional "validationMessage" string ""
                    |> optional "isMandatory" bool False
                    |> resolve        
    in
        map Question (lazy (\_ -> decodeQuestionRecord))


decodeOptions : Decoder (List QuestionOption)
decodeOptions = list (lazy (\_ -> decodeOption))

decodeOption : Decoder QuestionOption
decodeOption = 
    succeed QuestionOption
        |> optional "id" string ""
        |> required "text" string
        |> optional "isComplianceCheckNeeded" bool False
        |> optional "followUpQuestions" (lazy (\_ -> decodeQuestions)) []
        |> hardcoded False -- active

decodeSelectType : Decoder Bool
decodeSelectType =
    int |> map (\i ->
        case i of
            0 -> False 
            1 -> True
            _ -> False)