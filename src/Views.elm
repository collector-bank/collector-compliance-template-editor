module Views exposing (rootView)

import ViewComponent exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes as HA exposing (..)
import Html.Events exposing (..)
import QuestionTemplate.Model exposing (..)
import JsonModel.Serialization exposing (toJson)
import JsonModel.Deserialization exposing (fromJson)
import ViewHelpers exposing (..)
import Optics exposing (..)
import Monocle.Lens exposing (..)
import Monocle.Optional exposing (..)
import Optics exposing (..)
import Model exposing (..)
import OpticsNew exposing (..)
import QuestionTemplate.Views exposing (..)
import Question.OptionDetailsView exposing(..)
import QuestionCategory.DetailsView exposing (..)
import Question.QuestionDetailsView exposing (..)
import JsonModel.DetailsView exposing (..)
import Routing exposing (..)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid

panelNoHeader content =
    div [ class "card", style "margin" "5px", style "width" "100%", style "background-color" "#ddd" ] 
        [ div [ class "card-body" ] content ]


menuView : ViewComponent MenuState Model Msg
menuView = eval <| \model modelTraits focus menuState ->
    let   
        nullProduct = { id = "", name = "" }
        productsAndEmpty = nullProduct :: menuState.products
        isProductSelected : ProductDefinition -> Bool
        isProductSelected product = product.id == menuState.selectedProduct
        productTitle product = if product == nullProduct then "" else product.name ++ " (" ++ product.id ++ ")"
        updateSelectedProduct newSelectedProduct = modelTraits.makeMsg <| (focus |> composeFocus selectedProductOfMenuState).optional.set newSelectedProduct
        productToOption : ProductDefinition -> Html Msg
        productToOption product = option [ HA.value product.id, HA.selected (isProductSelected product), disabled (isProductSelected product) ] [ text (productTitle product) ]
        productDropdown : Html Msg
        productDropdown = select [ onInput updateSelectedProduct, class "form-control" ] (List.map productToOption productsAndEmpty)
        loadQuestionTemplate : Msg
        loadQuestionTemplate = LoadQuestionTemplate menuState.selectedProduct
        saveQuestionTemplate : Msg
        saveQuestionTemplate = SaveQuestionTemplate menuState.selectedProduct
        makeFormGroup : Maybe String -> Html Msg -> Html Msg
        makeFormGroup maybeLabel child = 
            let 
                children =
                    case maybeLabel of
                        Just label -> [ Html.label [ class "my-1 mr-sm-2"] [ text label ] , child ]
                        Nothing -> [ child ]
            in
                div [ class "form-group my-1 mr-sm-2" ] children
    in
        panelNoHeader <|
                [ span [] 
                    [ Html.form [ class "form-inline" ] 
                        [ makeFormGroup (Just "Product") <| productDropdown
                        , makeFormGroup Nothing <| button [ class "btn btn-primary", type_ "button", onClick loadQuestionTemplate, disabled (isProductSelected nullProduct) ] [ text "Load"]      
                        , makeFormGroup Nothing <| button [ class "btn btn-primary", type_ "button", onClick saveQuestionTemplate, disabled (isProductSelected nullProduct) ] [ text "Save"]
                        ]
                    ]
                ]

rootView : Model -> Html Msg
rootView model = 
    let
        modelTraits = { makeMsg = UpdateModel, makeIdMsg = UpdateModel << withNewId }
        menuView_ = menuView model modelTraits menuStateOfModel
        questionTemplateView_ = questionTemplateView model modelTraits questionTemplateOfModel
        questionDetailsView_ = 
            case routeToSelectedEntity model.route of 
                Category focus -> questionCategoryDetailsView model modelTraits focus
                Model.Question focus -> questionDetailsView model modelTraits focus
                Option focus -> optionDetailsView model modelTraits focus
                _ -> text ""
        jsonModelView_ = jsonModelView model modelTraits questionTemplateOfModel
    in
        Grid.container [ style "background-color" "#333", style "max-width" "100%" ]  -- #e10075,  #6b1faf
            [ CDN.stylesheet -- creates an inline style node with the Bootstrap CSS
            , div [ class "row" ] 
                [ div [ class "col" ] 
                    [ menuView_ ]
                ]
            , div [ class "row" ]
                [ div [ class "col-7" ]
                    [ questionTemplateView_ ]
                , div [ class "col-5"] 
                    [ questionDetailsView_  ]
                ]
            , div [ class "row"]
                [ div [ class "col" ] 
                    [ jsonModelView_ ]
                ]                
            ]
