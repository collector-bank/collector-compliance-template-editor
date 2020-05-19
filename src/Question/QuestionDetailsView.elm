module Question.QuestionDetailsView exposing (..)

import Question.Model exposing(..)
import Question.Optics exposing(..)
import ViewComponent exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import OpticsCore exposing (..)
import ViewHelpers exposing (..)
import Monocle.Optional as Optional
        
questionDetailsView : ViewComponent Question am msg
questionDetailsView = eval <| \model modelTraits focus (Question current) ->
    let
        updateQuestionType qtstr = 
            let 
                qt = 
                    case qtstr of
                        "freetext" -> makeFreeTextQuestion
                        "select"   -> makeSelectQuestion
                        "country"  -> makeCountryQuestion
                        "beneficialowners" -> makeBeneficialOwnersQuestion
                        _          -> makeQuestionGroup
            in
                modelTraits.makeMsg <| (focus |> composeFocus questionTypeOfQuestion).optional.set qt
        updateQuestionTitle newTitle = modelTraits.makeMsg <| (focus |> composeFocus titleOfQuestion).optional.set newTitle
        updateQuestionExtendedTitle newTitle = modelTraits.makeMsg <| (focus |> composeFocus extendedTitleOfQuestion).optional.set newTitle
        updateQuestionDescription newDescription = modelTraits.makeMsg <| (focus |> composeFocus descriptionOfQuestion).optional.set newDescription
        updateQuestionExtendedDescription newDescription = modelTraits.makeMsg <| (focus |> composeFocus extendedDescriptionOfQuestion).optional.set newDescription
    in
        panel "Question Details" <|
            div []
                [ formSelectInput "questionTypeInput" "Question Type" updateQuestionType
                    [
                        ("Free Text", "freetext", isFreeText current.questionType),
                        ("Select",    "select"  , isSelectQuestion current.questionType),
                        ("Country",   "country",  isCountryQuestion current.questionType),
                        ("Group",     "group",    isQuestionGroup current.questionType),
                        ("Beneficial Owners", "beneficialowners", isBeneficialOwnersQuestion current.questionType)
                    ]
                    , formTextInput "titleInput" "Title" current.title Nothing updateQuestionTitle
                    , formTextInput "extendedTitleInput" "Extended Title" current.extendedTitle (Just "A formatted title that may use html tags") updateQuestionExtendedTitle
                    , formTextInput "descriptionInput" "Description" current.description Nothing updateQuestionDescription
                    , formTextInput "extendedDescriptionInput" "Extended Description" current.extendedDescription (Just "A formatted description that may use html tags") updateQuestionExtendedDescription
                , questionTypeSpecificFieldsView model modelTraits (focus |> composeFocus questionTypeOfQuestion)
                ]                        

questionTypeSpecificFieldsView : ViewComponent QuestionType am msg
questionTypeSpecificFieldsView = eval <| \model modelTraits focus questionType -> 
    let
        updateQuestionMaxLength newMaxLen = modelTraits.makeMsg <| (focus |> composeFocus freeTextQuestionOfQuestion |> composeFocus maxLengthOfFreeTextQuestion).optional.set newMaxLen
        addSubQuestion = modelTraits.makeIdMsg <| \id -> (focus |> composeFocus questionGroupOfQuestion |> composeFocus questionsOfGroupQuestion |> composeFocus (questionOfQuestionList id)).optional.set (makeQuestion id)                           
        addOption = modelTraits.makeIdMsg <| \id -> (focus |> composeFocus selectQuestionOfQuestion |> composeFocus optionsOfSelectQuestion |> composeFocus (optionOfOptionsList id)).optional.set (makeOption id) 
        toggleAllowMultipleSelect = modelTraits.makeMsg <| Optional.modify (focus |> composeFocus selectQuestionOfQuestion |> composeFocus allowMultipleOfSelectQuestion).optional not
        toggleAllowMultipleCountry = modelTraits.makeMsg <| Optional.modify (focus |> composeFocus countryQuestionOfQuestion |> composeFocus allowMultipleOfCountryQuestion).optional not
        updateMaxRepeat newMaxRepeat = modelTraits.makeMsg <| (focus |> composeFocus questionGroupOfQuestion |> composeFocus maxRepeatOfGroupQuestion).optional.set newMaxRepeat
        updateValidationMessage newValidationMessage = modelTraits.makeMsg <| (focus |> composeFocus countryQuestionOfQuestion |> composeFocus validationMessageOfCountryQuestion).optional.set newValidationMessage
        toggleIsMandatory = modelTraits.makeMsg <| Optional.modify (focus |> composeFocus freeTextQuestionOfQuestion |> composeFocus isMandatoryOfFreeTextQuestion).optional not
    in
        case questionType of 
            FreeTextQuestion ftq -> 
                div [] 
                    [ formTextInput "maxLengthInput" "The maximum answer length" ftq.maxLength Nothing updateQuestionMaxLength
                    , formCheckBoxInput "isMandatory" "Mandatory" ftq.isMandatory toggleIsMandatory
                    ]                
            SelectQuestion sq -> 
                div [] 
                    [ formCheckBoxInput "allowMultipleInput" "Allow Multiple Choices" sq.allowMultiple toggleAllowMultipleSelect
                    , button [ class "btn btn-outline-primary btn-sm", onClick addOption ]
                        [ text "Add Option" ] 
                    ]
            CountryQuestion cq ->
                div [] 
                    [ formTextInput "validationMessage" "Validation Message" cq.validationMessage (Just "This message will be displayed when the country is not valid") updateValidationMessage
                    , formCheckBoxInput "allowMultipleInput" "Allow Multiple Choices" cq.allowMultiple toggleAllowMultipleCountry
                    ]
            QuestionGroup qs -> 
                div []
                    [ formTextInput "maxRepeatInput" "Max Repeat" qs.maxRepeat (Just "The maximum times the questions in the group may be asked. Zero means unlimited") updateMaxRepeat
                    , button [ class "btn btn-outline-primary btn-sm", onClick addSubQuestion ]
                        [ text "Add Sub Question" ]                                
                    ]
            BeneficialOwnersQuestion ->
                div [] []
