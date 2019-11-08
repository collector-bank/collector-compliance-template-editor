module Question.OptionDetailsView exposing (..)

import Question.Model exposing(..)
import Question.Optics exposing(..)
import ViewComponent exposing (..)
import Html exposing (..)
import Optics exposing (..)
import ViewHelpers exposing (..)
import Monocle.Optional as Optional

optionDetailsView : ViewComponent QuestionOption am msg
optionDetailsView = eval <| \model modelTraits focus option ->
    let
        updateOptionText s = modelTraits.makeMsg <| (focus |> composeFocus textOfOption).optional.set s
        toggleIsComplianceCheckNeeded = modelTraits.makeMsg <| Optional.modify (focus |> composeFocus isComplianceCheckNeededOfOption).optional not
    in
        panel "Option Details" <|
            div [] 
                [ formTextInput "optionTextInput" "Text" option.text Nothing updateOptionText
                , formCheckBoxInput "isComplianceCheckNeededInput" "Compliance Check Needed?" option.isComplianceCheckNeeded toggleIsComplianceCheckNeeded
                ]