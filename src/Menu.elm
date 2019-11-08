module Menu exposing (MenuState, MenuMsg(..), menuView, productsOfMenuState, parseProducts)

import ViewComponent exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import QuestionTemplate.Model exposing (..)
import JsonModel.Serialization exposing (toJson)
import JsonModel.Deserialization exposing (fromJson)
import ViewHelpers exposing (..)
import Optics exposing (..)
import Monocle.Lens exposing (..)
import Monocle.Optional exposing (..)
import Optics exposing (..)
import Json.Decode as D exposing (..)
import Json.Decode.Pipeline as DP exposing (..)

type MenuMsg = LoadQuestionTemplate String
             | SaveQuestionTemplate String

type alias ProductDefinition = { id : String, name : String }

type alias MenuState = 
    { products : List ProductDefinition,
      selectedProduct : String
    }

parseProducts : String -> Result Error (List ProductDefinition)
parseProducts = decodeString (D.list decodeProduct)

decodeProduct : Decoder ProductDefinition
decodeProduct = 
    succeed ProductDefinition
        |> DP.required "id" string
        |> DP.required "name" string

panelNoHeader content =
    div [ class "card", style "margin" "5px", style "width" "100%", style "background-color" "#ddd" ] 
        [ div [ class "card-body" ] content ]


selectedProductOfMenuState : Focus MenuState String
selectedProductOfMenuState = {
        optional = Lens (\s -> s.selectedProduct) (\t s -> { s | selectedProduct = t }) |> Monocle.Optional.fromLens,
        path = ["selectedProduct"]
    }

productsOfMenuState : Focus MenuState (List ProductDefinition)
productsOfMenuState = {
        optional = Lens (\s -> s.products) (\t s -> { s | products = t }) |> Monocle.Optional.fromLens,
        path = ["products"]
    }

menuView : (MenuMsg -> msg) -> ViewComponent MenuState am msg
menuView makeMenuMsg = eval <| \model modelTraits focus menuState ->
    let   
        updateSelectedProduct newSelectedProduct = modelTraits.makeMsg <| (focus |> composeFocus selectedProductOfMenuState).optional.set newSelectedProduct
        productToOption : ProductDefinition -> Html msg
        productToOption product = option [ Html.Attributes.value product.id ] [ text product.name ]
        productDropdown : Html msg
        productDropdown = select [ onInput updateSelectedProduct, class "form-control" ] (List.map productToOption menuState.products)
        loadQuestionTemplate = makeMenuMsg <| LoadQuestionTemplate menuState.selectedProduct
        saveQuestionTemplate = makeMenuMsg <| SaveQuestionTemplate menuState.selectedProduct
    in
        panelNoHeader <|
                [ span [] 
                    [ Html.form [ class "form-inline" ] 
                        [ productDropdown
                        , button [ class "btn btn-primary my-1 mr-sm-2", type_ "button", onClick loadQuestionTemplate ] [ text "Load"]      
                        , button [ class "btn btn-primary my-1 mr-sm-2", type_ "button", onClick saveQuestionTemplate ] [ text "Save"]
                        ]
                    ]
                ]