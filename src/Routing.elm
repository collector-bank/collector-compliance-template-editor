module Routing exposing (..)

import Url exposing (Url)

type RoutePart
    = CategoryRoute String
    | QuestionRoute String
    | OptionsRoute String
    | GroupQuestion String
    | FollowUpQuestion String

type alias Route = List RoutePart

parseLocation : Url -> List RoutePart
parseLocation location = 
    let
        parse xss =
            case xss of 
                "categories"         :: categoryName :: xs    -> CategoryRoute    categoryName :: parse xs
                "questions"          :: questionId   :: xs    -> QuestionRoute    questionId   :: parse xs
                "subquestions"       :: questionId   :: xs    -> GroupQuestion    questionId   :: parse xs
                "options"            :: optionsId    :: xs    -> OptionsRoute     optionsId    :: parse xs
                "followup-questions" :: questionId   :: xs    -> FollowUpQuestion questionId   :: parse xs
                _ -> []
    in
        parse <| String.split "/" (Maybe.withDefault "" location.fragment)
