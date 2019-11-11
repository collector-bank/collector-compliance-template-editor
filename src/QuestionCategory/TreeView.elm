module QuestionCategory.TreeView exposing (questionCategoriesView)

import ViewComponent exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import ViewComponent exposing (..)
import Question.Model exposing (..)
import Question.TreeView exposing (..)
import Question.Optics exposing (questionOfQuestionList)
import Bootstrap.ListGroup as ListGroup
import OpticsCore exposing (..)
import ViewHelpers exposing (..)
import QuestionCategory.Model exposing (..)
import QuestionCategory.Optics exposing (..)
import Icons exposing (categoryIcon)

questionCategoriesView : ViewComponent (List QuestionCategory) am msg
questionCategoriesView = eval <| \model modelTraits focus questionCategories ->
    let
        makeQuestionCategoryView : QuestionCategory -> ListGroup.Item msg
        makeQuestionCategoryView questionCategory =
            let
                questionCategoryName = categoryTypeToString questionCategory.categoryType
                focus_ = focus |> composeFocus (questionCategoryOfQuestionCategoryList questionCategory.categoryType)
                setActiveFlag isActive = modelTraits.makeMsg <| (focus_ |> composeFocus activeOfQuestionCategory).optional.set isActive
                addNewQuestionToCategory = 
                    modelTraits.makeIdMsg <| \id -> (focus_ |> composeFocus questionsOfQuestionCategory |> composeFocus (questionOfQuestionList id)).optional.set (makeQuestion id)
                address = href ("#" ++ String.join "/" focus_.path)                
            in
                ListGroup.li [ ListGroup.attrs [style "border" "0"] ] 
                    [ p [ onMouseOver (setActiveFlag True), onMouseOut (setActiveFlag False) ]
                        [ treeIcon Icons.categoryIcon questionCategory.active
                        , Html.a [ address ] [ text questionCategoryName ]
                        , treeViewButtons [("Add Question", addNewQuestionToCategory)] questionCategory
                        ]
                    , questionsView model modelTraits (focus_ |> composeFocus questionsOfQuestionCategory)            
                    ]                
    in
        div [] 
            [ ListGroup.ul 
                <| List.map makeQuestionCategoryView questionCategories
            ]
