module QuestionTemplate.Optics exposing (..)

import QuestionTemplate.Model exposing (..)
import QuestionCategory.Model exposing (..)
import Optics exposing (..)
import Monocle.Optional exposing (Optional)
import Monocle.Lens exposing (Lens)

categoriesOfQuestionTemplate : Focus QuestionTemplate (List QuestionCategory)
categoriesOfQuestionTemplate = {
        optional = Lens .categories (\newValue o -> { o | categories = newValue }) |> Monocle.Optional.fromLens,
        path = [ "categories" ]
    }

nameOfQuestionTemplate : Focus QuestionTemplate String
nameOfQuestionTemplate = {
        optional = Lens .name (\newValue o -> { o | name = newValue }) |> Monocle.Optional.fromLens,
        path = [ "name" ]
    }
