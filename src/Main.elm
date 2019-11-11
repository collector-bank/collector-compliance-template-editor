module Main exposing (..)

import Html exposing (Html, div, text, h1)
import Html.Attributes exposing (style, class)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Browser exposing (Document,application)
import Browser.Navigation as Nav
import Routing exposing (..)
import Random exposing (initialSeed)
import QuestionCategory.Model exposing (..)
import QuestionCategory.Optics exposing (..)
import OpticsCore exposing (..)
import QuestionTemplate.Model exposing (..)
import QuestionTemplate.Optics exposing (..)
import ViewHelpers exposing (..)
import Url exposing (Url)
import JsonModel.Deserialization exposing (fromJson)
import JsonModel.Serialization exposing (toJson)
import Ports exposing (..)
import Model exposing (..)
import Optics exposing (..)
import Deserialization exposing (..)
import Views exposing (rootView)

-- Program entry point

main : Program () Model Msg    
main = Browser.application { 
         init = init, 
         view = view, 
         update = update, 
         subscriptions = subscriptions, 
         onUrlChange = ChangedUrl, 
         onUrlRequest = ClickedLink 
       }

-- Init

init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags location key = 
    let
        initialModel = 
            { route = parseLocation location
            , key = key
            , uuidSeed = initialSeed 0
            , questionTemplate =  
                { name = ""                    
                , categories= [ makeQuestionCategory PEP, makeQuestionCategory AML ]
                }
            , menuState = 
                { products = [ ]
                , selectedProduct = ""
                }
            }
    in
        (initialModel, Cmd.none)


-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model = 
    Sub.batch 
        [ questionTemplateLoaded (\data -> 
            case fromJson data of
                Ok questionTemplate -> QuestionTemplateLoaded questionTemplate
                Err error -> NoAction)
        , questionTemplateSaved (\_ -> QuestionTemplateSaved)
        , productsLoaded (\data -> 
            case parseProducts data of
                Ok products -> UpdateModel <| (menuStateOfModel |> composeFocus productsOfMenuState).optional.set products
                Err error -> NoAction)
        ]

-- Update

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case Debug.log "MESSAGE: " msg of 
        UpdateModel f -> 
            (f model, Cmd.none)
        ClickedLink urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )
                Browser.External href ->
                    ( model, Nav.load href )
        ChangedUrl url -> 
            ({ model | route = parseLocation url }, Cmd.none)
        LoadQuestionTemplate product ->
            (model, loadQuestionTemplate product)
        SaveQuestionTemplate product ->
            (model, saveQuestionTemplate (product, toJson model.questionTemplate))
        QuestionTemplateLoaded questionTemplate ->
            ({ model | questionTemplate = questionTemplate }, Cmd.none)
        QuestionTemplateSaved ->
            (model, Cmd.none)
        NoAction ->
            (model, Cmd.none)

-- Views

view : Model -> Document Msg
view model = { title = "Compliance Template Editor", body = [ rootView model ] }
