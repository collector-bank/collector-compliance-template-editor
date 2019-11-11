module Main exposing (..)

import Html exposing (Html, div, text, h1)
import Html.Attributes exposing (style, class)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Browser exposing (Document,application)
import Browser.Navigation as Nav
import Monocle.Optional exposing (Optional)
import Monocle.Lens exposing (Lens)
import Routing exposing (..)
import Uuid exposing (Uuid, uuidGenerator)
import Random exposing (Seed, step, initialSeed)
import Question.Model exposing (..)
import Question.Optics exposing (..)
import Question.QuestionDetailsView exposing (..)
import Question.OptionDetailsView exposing(..)
import QuestionCategory.Model exposing (..)
import QuestionCategory.Optics exposing (..)
import QuestionCategory.DetailsView exposing (..)
import Optics exposing (..)
import JsonModel.DetailsView exposing (..)
import QuestionTemplate.Model exposing (..)
import QuestionTemplate.Views exposing (..)
import QuestionTemplate.Optics exposing (..)
import ViewHelpers exposing (..)
import Url exposing (Url)
import Menu exposing (..)
import JsonModel.Deserialization exposing (fromJson)
import JsonModel.Serialization exposing (toJson)
import Ports exposing (..)
import Model exposing (..)
import OpticsNew exposing (..)
import Deserialization exposing (..)

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


-- Helpers


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
        MenuAction action ->
            case action of            
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
view model = 
    let
        rootView : Html Msg
        rootView = 
            let
                modelTraits = { makeMsg = UpdateModel, makeIdMsg = UpdateModel << withNewId }
                menuView_ = menuView MenuAction model modelTraits menuStateOfModel
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
    in
        { title = "Compliance Template Editor", body = [ rootView ] }
