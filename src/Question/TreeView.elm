module Question.TreeView exposing (..)

import Question.Model exposing(..)
import Question.Optics exposing(..)
import ViewComponent exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Bootstrap.ListGroup as ListGroup
import Optics exposing (..)
import ViewHelpers exposing (..)
import Monocle.Optional as Optional exposing (modify)
import Icons

questionsView : ViewComponent (List Question) am msg
questionsView = eval <| \model modelTraits focus questions ->
    let 
        makeQuestionView (Question question) = 
            let
                focus_ = focus |> composeFocus (questionOfQuestionList question.id)
                setActiveFlag isActive = modelTraits.makeMsg <| (focus_ |> composeFocus activeOfQuestion).optional.set isActive
                address = href ("#" ++ String.join "/" focus_.path)
                deleteQuestionMsg = modelTraits.makeMsg <| focus.optional.set <| List.filter (\(Question q) -> q.id /= question.id) questions
                toggleCollapsed = modelTraits.makeMsg <| Optional.modify (focus_ |> composeFocus collapsedOfQuestion).optional not
            in
                ListGroup.li [ ListGroup.attrs [style "border" "0"] ]
                    [ p [ onMouseOver (setActiveFlag True), onMouseOut (setActiveFlag False) ]
                        [ treeIcon2 Icons.questionIconExpanded Icons.questionIconCollapsed question.active question.collapsed toggleCollapsed
                        , Html.a [ address  ] [ text question.title ]
                        , treeViewButtons [("Delete Question", deleteQuestionMsg)] question
                        ]
                    , if question.collapsed then text "" else subQuestionsView model modelTraits (focus |> composeFocus (questionOfQuestionList question.id))
                    ]                                        
    in
        ListGroup.ul <| 
            List.map makeQuestionView questions

subQuestionsView : ViewComponent Question am msg
subQuestionsView = eval <| \model modelTraits focus (Question question) ->
    case question.questionType of 
        QuestionGroup _ ->
            questionsView model modelTraits (focus |> composeFocus questionTypeOfQuestion |> composeFocus questionGroupOfQuestion |> composeFocus questionsOfGroupQuestion)
        SelectQuestion _ ->
            optionsView model modelTraits (focus |> composeFocus questionTypeOfQuestion |> composeFocus selectQuestionOfQuestion |> composeFocus optionsOfSelectQuestion)
        _ -> text ""

optionsView : ViewComponent (List QuestionOption) am msg
optionsView = eval <| \model modelTraits focus options ->
    let
        makeOptionView : QuestionOption -> ListGroup.Item msg
        makeOptionView option = 
            let
                focus_ = focus |> composeFocus (optionOfOptionsList option.id)
                setActiveFlag isActive = modelTraits.makeMsg <| (focus_ |> composeFocus activeOfOption).optional.set isActive
                address = href ("#" ++ String.join "/" focus_.path)
                deleteOptionMsg = modelTraits.makeMsg <| focus.optional.set <| List.filter (\o -> o.id /= option.id) options
                addFollowUpMsg = modelTraits.makeIdMsg <| \id -> (focus_ |> composeFocus followUpQuestionsOfOption |> composeFocus (questionOfQuestionList id)).optional.set (makeQuestion id)
            in                
                ListGroup.li [ ListGroup.attrs [style "border" "0"] ] 
                    [ p [ onMouseOver (setActiveFlag True), onMouseOut (setActiveFlag False) ]
                        [ treeIcon Icons.check_circle option.active
                        , Html.a [ address  ] [ text option.text ]
                        , treeViewButtons 
                            [("Delete Option", deleteOptionMsg)
                            ,("Add Followup Question", addFollowUpMsg)
                            ] 
                            option
                        ]
                    , questionsView model modelTraits (focus |> composeFocus (optionOfOptionsList option.id) |> composeFocus followUpQuestionsOfOption)
                    ]
    in
        ListGroup.ul <| 
            List.map makeOptionView options
