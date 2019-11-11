module QuestionCategory.Optics exposing (..)

import Question.Model exposing (..)
import QuestionCategory.Model exposing (..)
import OpticsCore exposing (..)
import Monocle.Lens exposing (..)
import Monocle.Optional exposing (..)

questionsOfQuestionCategory : Focus QuestionCategory (List Question)
questionsOfQuestionCategory = {
        optional = Lens .questions (\ql qc -> { qc | questions = ql }) |> Monocle.Optional.fromLens,
        path = [ "questions" ]
    }

activeOfQuestionCategory : Focus QuestionCategory Bool 
activeOfQuestionCategory = {
        optional = Lens .active (\a qc -> { qc | active = a }) |> Monocle.Optional.fromLens,
        path = [ "active" ]
    }

questionCategoryOfQuestionCategoryList : CategoryType -> Focus (List QuestionCategory) QuestionCategory
questionCategoryOfQuestionCategoryList categoryType = {
        optional = elementOfElementList (\c -> c.categoryType == categoryType),
        path = [ categoryTypeToString categoryType ]
    }

titleOfQuestionCategory : Focus QuestionCategory String 
titleOfQuestionCategory = {
        optional = Lens .title (\a qc -> { qc | title = a }) |> Monocle.Optional.fromLens,
        path = [ "title" ]
    }

extendedTitleOfQuestionCategory : Focus QuestionCategory String 
extendedTitleOfQuestionCategory = {
        optional = Lens .extendedTitle (\a qc -> { qc | extendedTitle = a }) |> Monocle.Optional.fromLens,
        path = [ "extended-title" ]
    }

descriptionOfQuestionCategory : Focus QuestionCategory String 
descriptionOfQuestionCategory = {
        optional = Lens .description (\a qc -> { qc | description = a }) |> Monocle.Optional.fromLens,
        path = [ "description" ]
    }

extendedDescriptionOfQuestionCategory : Focus QuestionCategory String 
extendedDescriptionOfQuestionCategory = {
        optional = Lens .extendedDescription (\a qc -> { qc | extendedDescription = a }) |> Monocle.Optional.fromLens,
        path = [ "extended-description" ]
    }
