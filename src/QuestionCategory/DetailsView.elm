module QuestionCategory.DetailsView exposing (..)

import QuestionCategory.Model exposing(..)
import QuestionCategory.Optics exposing(..)
import ViewComponent exposing (..)
import Html exposing (..)
import OpticsCore exposing (..)
import ViewHelpers exposing (..)
import Monocle.Optional as Optional

questionCategoryDetailsView : ViewComponent QuestionCategory am msg
questionCategoryDetailsView = eval <| \model modelTraits focus questionCategory ->
    let
        updateTitle s = modelTraits.makeMsg <| (focus |> composeFocus titleOfQuestionCategory).optional.set s
        updateExtendedTitle s = modelTraits.makeMsg <| (focus |> composeFocus extendedTitleOfQuestionCategory).optional.set s
        updateDescription s = modelTraits.makeMsg <| (focus |> composeFocus descriptionOfQuestionCategory).optional.set s
        updateExtendedDescription s = modelTraits.makeMsg <| (focus |> composeFocus extendedDescriptionOfQuestionCategory).optional.set s
    in
        panel "Category Details" <|
            div []       
                [ formTextInput "titleInput" "Title" questionCategory.title Nothing updateTitle
                , formTextInput "extendedTitleInput" "Extended Title" questionCategory.extendedTitle (Just "A formatted title that may use html tags") updateExtendedTitle
                , formTextInput "descriptionInput" "Description" questionCategory.description Nothing updateDescription
                , formTextInput "extendedDescriptionInput" "Extended Description" questionCategory.extendedDescription (Just "A formatted description that may use html tags") updateExtendedDescription
                ]