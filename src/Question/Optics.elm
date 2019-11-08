module Question.Optics exposing(..)

import Question.Model exposing(..)
import Monocle.Lens exposing (..)
import Monocle.Optional exposing (..)
import Optics exposing (..)

freeTextQuestionOfQuestion : Focus QuestionType FreeTextQuestionFields 
freeTextQuestionOfQuestion = 
    { 
        optional = Optional (\qt -> 
                case qt of 
                    FreeTextQuestion ftq -> Just ftq
                    _ -> Nothing 
            ) (\newq _ -> FreeTextQuestion newq),
        path = []
    }

maxLengthOfFreeTextQuestion : Focus FreeTextQuestionFields String
maxLengthOfFreeTextQuestion = {
        optional = Lens .maxLength (\newValue o -> { o | maxLength = newValue }) |> Monocle.Optional.fromLens,
        path = ["max-length"]
    }

isMandatoryOfFreeTextQuestion : Focus FreeTextQuestionFields Bool
isMandatoryOfFreeTextQuestion = {
        optional = Lens .isMandatory (\newValue o -> { o | isMandatory = newValue }) |> Monocle.Optional.fromLens,
        path = ["mandatory"]
    }

questionGroupOfQuestion : Focus QuestionType QuestionGroupFields
questionGroupOfQuestion = 
    { 
        optional = Optional (\qt -> 
                case qt of 
                    QuestionGroup gq -> Just gq
                    _ -> Nothing 
            ) (\newq _ -> QuestionGroup newq),
        path = []
    }

questionsOfGroupQuestion : Focus QuestionGroupFields (List Question)
questionsOfGroupQuestion = {
        optional = Lens .questions (\newValue o -> { o | questions = newValue }) |> Monocle.Optional.fromLens,
        path = ["subquestions"]
    }

maxRepeatOfGroupQuestion : Focus QuestionGroupFields String
maxRepeatOfGroupQuestion = {
        optional = Lens .maxRepeat (\newValue o -> { o | maxRepeat = newValue }) |> Monocle.Optional.fromLens,
        path = ["max-repeat"]
    }

textOfOption : Focus QuestionOption String
textOfOption = {
        optional = Lens (\o -> o.text) (\newValue o -> { o | text = newValue }) |> Monocle.Optional.fromLens,
        path = ["text"]
    }

activeOfOption : Focus QuestionOption Bool
activeOfOption = {
        optional = Lens (\o -> o.active) (\newValue o -> { o | active = newValue }) |> Monocle.Optional.fromLens,
        path = ["active"]
    }

followUpQuestionsOfOption : Focus QuestionOption (List Question)
followUpQuestionsOfOption = {
        optional = Lens (\o -> o.followUpQuestions) (\fuq o -> { o | followUpQuestions = fuq }) |> Monocle.Optional.fromLens,
        path = ["followup-questions"]
    }

isComplianceCheckNeededOfOption : Focus QuestionOption Bool
isComplianceCheckNeededOfOption = {
        optional = Lens (\o -> o.isComplianceCheckNeeded) (\newValue o -> { o | isComplianceCheckNeeded = newValue }) |> Monocle.Optional.fromLens,
        path = ["is-compliancecheck-Needed"]
    }

optionOfOptionsList : String -> Focus (List QuestionOption) QuestionOption
optionOfOptionsList optionId = {
        optional = elementOfElementList (\o -> o.id == optionId),
        path = [ optionId ]
    }


selectQuestionOfQuestion : Focus QuestionType SelectQuestionFields
selectQuestionOfQuestion = 
    { 
        optional = Optional (\qt -> 
                case qt of 
                    SelectQuestion sq -> Just sq
                    _ -> Nothing 
            ) (\newq _ -> SelectQuestion newq),
        path = []
    }

optionsOfSelectQuestion : Focus SelectQuestionFields (List QuestionOption)
optionsOfSelectQuestion = {
        optional = Lens (\o -> o.options) (\newValue o -> { o | options = newValue }) |> Monocle.Optional.fromLens,
        path = ["options"]
    }

allowMultipleOfSelectQuestion : Focus SelectQuestionFields Bool
allowMultipleOfSelectQuestion = {
        optional = Lens (\o -> o.allowMultiple) (\newValue o -> { o | allowMultiple = newValue }) |> Monocle.Optional.fromLens,
        path = ["allow-multiple"]
    }

countryQuestionOfQuestion : Focus QuestionType CountryQuestionFields  
countryQuestionOfQuestion =
    { 
        optional = Optional (\qt -> 
                case qt of 
                    CountryQuestion cq -> Just cq
                    _ -> Nothing 
            ) (\newq _ -> CountryQuestion newq),
        path = ["country-question"]
    }

allowMultipleOfCountryQuestion : Focus CountryQuestionFields Bool
allowMultipleOfCountryQuestion = {
        optional = Lens (\o -> o.allowMultiple) (\newValue o -> { o | allowMultiple = newValue }) |> Monocle.Optional.fromLens,
        path = ["allow-multiple"]
    }

validationMessageOfCountryQuestion : Focus CountryQuestionFields String
validationMessageOfCountryQuestion = {
        optional = Lens (\o -> o.validationMessage) (\newValue o -> { o | validationMessage = newValue }) |> Monocle.Optional.fromLens,
        path = ["validation-message"]
    }

questionTypeOfQuestion : Focus Question QuestionType
questionTypeOfQuestion = {
            optional = Lens (\(Question q) -> q.questionType) (\qt (Question q) -> Question { q | questionType = qt }) |> Monocle.Optional.fromLens,
            path = [] 
        }

activeOfQuestion : Focus Question Bool
activeOfQuestion = {
        optional = Lens (\(Question q) -> q.active) (\a (Question q) -> Question { q | active = a }) |> Monocle.Optional.fromLens,
        path = ["active"]
    }

collapsedOfQuestion : Focus Question Bool
collapsedOfQuestion = {
        optional = Lens (\(Question q) -> q.collapsed) (\a (Question q) -> Question { q | collapsed = a }) |> Monocle.Optional.fromLens,
        path = ["collapsed"]
    }

titleOfQuestion : Focus Question String
titleOfQuestion = {
        optional = Lens (\(Question q) -> q.title) (\t (Question q) -> Question { q | title = t }) |> Monocle.Optional.fromLens,
        path = ["title"]
    }

extendedTitleOfQuestion : Focus Question String
extendedTitleOfQuestion = {
        optional = Lens (\(Question q) -> q.extendedTitle) (\t (Question q) -> Question { q | extendedTitle = t }) |> Monocle.Optional.fromLens,
        path = ["extended-title"]
    }

descriptionOfQuestion : Focus Question String
descriptionOfQuestion = {
        optional = Lens (\(Question q) -> q.description) (\d (Question q) -> Question { q | description = d }) |> Monocle.Optional.fromLens,
        path = ["description"]
    }

extendedDescriptionOfQuestion : Focus Question String
extendedDescriptionOfQuestion = {
        optional = Lens (\(Question q) -> q.extendedDescription) (\d (Question q) -> Question { q | extendedDescription = d }) |> Monocle.Optional.fromLens,
        path = ["extended-description"]
    }

questionOfQuestionList : String -> Focus (List Question) Question
questionOfQuestionList questionId = {
        optional = elementOfElementList (\(Question q) -> q.id == questionId),
        path = [ questionId ]
    }

questionOfQuestionGroup : String -> Focus Question Question
questionOfQuestionGroup questionId =
     questionTypeOfQuestion |> 
     composeFocus questionGroupOfQuestion |> 
     composeFocus questionsOfGroupQuestion |>
     composeFocus (questionOfQuestionList questionId)

