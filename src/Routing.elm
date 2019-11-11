module Routing exposing (..)

import Url exposing (Url)
import Model exposing (SelectedEntity(..), RoutePart(..), Route, Model)
import OpticsNew exposing (..)
import OpticsCore exposing (..)
import QuestionTemplate.Optics exposing (..)
import QuestionCategory.Optics exposing (..)
import Question.Optics exposing (..)
import QuestionCategory.Model exposing (..)
import Question.Model exposing (..)

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

routeToSelectedEntity : Route -> SelectedEntity Model
routeToSelectedEntity = 
    let
        categoryRouteToSelectedEntity : Route -> SelectedEntity Model
        categoryRouteToSelectedEntity route =
            case route of 
                (CategoryRoute categoryTypeName)::xs -> 
                    let
                        focus_ categoryType = questionTemplateOfModel |> composeFocus categoriesOfQuestionTemplate |> composeFocus (questionCategoryOfQuestionCategoryList categoryType)
                        makeResult categoryType = 
                            case xs of 
                                [] -> Category <| focus_ categoryType
                                _  -> questionRouteToSelectedEntity xs (focus_ categoryType)
                    in                
                        case String.toUpper categoryTypeName of
                            "PEP" -> makeResult PEP
                            "AML" -> makeResult AML
                            _ -> Debug.log ("No route found for category " ++ categoryTypeName) None
                _ -> None

        questionRouteToSelectedEntity : Route -> Focus a QuestionCategory -> SelectedEntity a
        questionRouteToSelectedEntity route focus =
            case route of 
                (QuestionRoute questionId)::xs -> 
                    let
                        focus_ = focus |> composeFocus questionsOfQuestionCategory |> composeFocus (questionOfQuestionList questionId)
                    in                
                        case xs of 
                            [] -> Model.Question <| focus_
                            _  -> subQuestionRouteToSelectedEntity xs focus_
                _ -> None

        subQuestionRouteToSelectedEntity : Route -> Focus a Question -> SelectedEntity a
        subQuestionRouteToSelectedEntity route focus =
            case route of 
                (OptionsRoute optionId)::xs -> 
                    let
                        focus_ = focus |> composeFocus questionTypeOfQuestion |> composeFocus selectQuestionOfQuestion |> composeFocus optionsOfSelectQuestion |> composeFocus (optionOfOptionsList optionId)
                    in                
                        case xs of 
                            [] -> Option <| focus_
                            _  -> optionRouteToSelectedEntity xs focus_
                (GroupQuestion questionId)::xs ->
                    let
                        focus_ = focus |> composeFocus questionTypeOfQuestion |> composeFocus questionGroupOfQuestion |> composeFocus questionsOfGroupQuestion |> composeFocus (questionOfQuestionList questionId)
                    in                
                        case xs of 
                            [] -> Model.Question <| focus_
                            _  -> subQuestionRouteToSelectedEntity xs focus_
                _ -> None

        optionRouteToSelectedEntity : Route -> Focus a QuestionOption -> SelectedEntity a
        optionRouteToSelectedEntity route focus =
            case route of 
                (FollowUpQuestion questionId)::xs -> 
                    let
                        focus_ = focus |> composeFocus followUpQuestionsOfOption |> composeFocus (questionOfQuestionList questionId)
                    in                
                        case xs of 
                            [] -> Model.Question <| focus_
                            _  -> subQuestionRouteToSelectedEntity xs focus_
                
                _ -> None
        
    in        
        categoryRouteToSelectedEntity 
