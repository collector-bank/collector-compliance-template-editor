module OpticsNew exposing (..)

import Model exposing (Model)
import QuestionTemplate.Model exposing (QuestionTemplate)
import Optics exposing (Focus)
import Menu exposing (MenuState)
import Monocle.Lens exposing (Lens)
import Monocle.Optional exposing (Optional)

-- Optics

questionTemplateOfModel : Focus Model QuestionTemplate
questionTemplateOfModel = {
        optional = Lens (\m -> m.questionTemplate) (\c m -> { m | questionTemplate = c }) |> Monocle.Optional.fromLens,
        path = [ ]
    }

menuStateOfModel : Focus Model MenuState
menuStateOfModel = {
        optional = Lens (\m -> m.menuState) (\c m -> { m | menuState = c }) |> Monocle.Optional.fromLens,
        path = [ ]
    }  
