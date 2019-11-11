module Menu exposing (menuView)

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

panelNoHeader content =
    div [ class "card", style "margin" "5px", style "width" "100%", style "background-color" "#ddd" ] 
        [ div [ class "card-body" ] content ]


menuView : (MenuMsg -> msg) -> ViewComponent MenuState am msg
menuView makeMenuMsg = eval <| \model modelTraits focus menuState ->
    let   
        nullProduct = { id = "", name = "" }
        productsAndEmpty = nullProduct :: menuState.products
        isProductSelected : ProductDefinition -> Bool
        isProductSelected product = product.id == menuState.selectedProduct
        productTitle product = if product == nullProduct then "" else product.name ++ " (" ++ product.id ++ ")"
        updateSelectedProduct newSelectedProduct = modelTraits.makeMsg <| (focus |> composeFocus selectedProductOfMenuState).optional.set newSelectedProduct
        productToOption : ProductDefinition -> Html msg
        productToOption product = option [ HA.value product.id, HA.selected (isProductSelected product), disabled (isProductSelected product) ] [ text (productTitle product) ]
        productDropdown : Html msg
        productDropdown = select [ onInput updateSelectedProduct, class "form-control" ] (List.map productToOption productsAndEmpty)
        loadQuestionTemplate = makeMenuMsg <| LoadQuestionTemplate menuState.selectedProduct
        saveQuestionTemplate = makeMenuMsg <| SaveQuestionTemplate menuState.selectedProduct
        makeFormGroup : Maybe String -> Html msg -> Html msg
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