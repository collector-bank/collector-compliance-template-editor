module Optics exposing (..)

import Model exposing (..)
import QuestionTemplate.Model exposing (QuestionTemplate)
import OpticsCore exposing (Focus, composeFocus)
import Monocle.Lens exposing (Lens)
import Monocle.Optional exposing (Optional)

-- Optics

questionTemplateOfModel : Focus Model QuestionTemplate
questionTemplateOfModel = {
        optional = Lens (\m -> m.questionTemplate) (\c m -> { m | questionTemplate = c }) |> Monocle.Optional.fromLens,
        path = [ ]
    }

menuStateOfModel : Focus Model MenuState
menuStateOfModel = {
        optional = Lens (\m -> m.menuState) (\c m -> { m | menuState = c }) |> Monocle.Optional.fromLens,
        path = [ ]
    }  

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

isBusyOfMenuState : Focus MenuState Bool
isBusyOfMenuState = {
        optional = Lens (\s -> s.isBusy) (\t s -> { s | isBusy = t }) |> Monocle.Optional.fromLens,
        path = ["isBusy"]
    }

isBusyOfModel : Focus Model Bool
isBusyOfModel = menuStateOfModel |> composeFocus isBusyOfMenuState