module QuestionTemplate.Views exposing (..)

import QuestionTemplate.Model exposing (..)
import QuestionTemplate.Optics exposing (..)
import QuestionCategory.TreeView exposing (..)
import ViewComponent exposing (..)
import Optics exposing (..)
import Html exposing (div, h1, text)
import ViewHelpers exposing (..)

questionTemplateView : ViewComponent QuestionTemplate am msg
questionTemplateView = eval <| \model modelTraits focus questionTemplate ->
    let
        updateQuestionTemplateName s = modelTraits.makeMsg <| (focus |> composeFocus nameOfQuestionTemplate).optional.set s
    in
        panel "Question Template Hierarchy" <| 
            div []
                [ formTextInput "questionTemplateNameInput" "Template Name" questionTemplate.name Nothing updateQuestionTemplateName
                , questionCategoriesView model modelTraits (focus |> composeFocus categoriesOfQuestionTemplate)
                ]
