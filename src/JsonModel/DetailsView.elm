module JsonModel.DetailsView exposing (..)

import ViewComponent exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import QuestionTemplate.Model exposing (..)
import JsonModel.Serialization exposing (toJson)
import JsonModel.Deserialization exposing (fromJson)
import ViewHelpers exposing (..)
import Json.Decode exposing (errorToString)

jsonModelView : ViewComponent QuestionTemplate am msg
jsonModelView = eval <| \model modelTraits focus questionTemplate ->
    let     
        updateModel : String -> msg
        updateModel s = 
            case fromJson s of
                Ok value -> modelTraits.makeMsg <| focus.optional.set value
                Err error -> modelTraits.makeMsg <| Debug.log ( "Error while parsing json: " ++ (errorToString error)) (\x -> x)  -- todo : display error
    in                
        panel2 "Json Representation" <|
            div [] 
                [ textarea [ rows 20, value (toJson questionTemplate), style "font-family" "monospace", onChange updateModel, style "width" "100%" ] []
                ]
