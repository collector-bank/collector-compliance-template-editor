module QuestionTemplate.Model exposing (..)

import QuestionCategory.Model exposing (QuestionCategory)

type alias QuestionTemplate = 
    { name : String
    , categories : List QuestionCategory
    }
