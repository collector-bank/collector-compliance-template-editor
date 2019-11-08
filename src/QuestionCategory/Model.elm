module QuestionCategory.Model exposing (..)

import Question.Model exposing (Question)

type CategoryType = AML | PEP

type alias QuestionCategory = 
    { title : String
    , extendedTitle : String
    , description : String
    , extendedDescription : String
    , categoryType : CategoryType
    , questions : List Question
    , active : Bool }

makeQuestionCategory categoryType = 
    { title = ""
    , extendedTitle = ""
    , description = ""
    , extendedDescription = ""
    , categoryType = categoryType
    , questions = []
    , active = False
    }

categoryTypeToString : CategoryType -> String 
categoryTypeToString ct = 
    case ct of
      AML -> "AML"
      PEP -> "PEP"  
