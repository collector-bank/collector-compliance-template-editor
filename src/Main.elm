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

-- Types

type alias Model = { route : Route, key : Nav.Key, uuidSeed : Seed, questionTemplate : QuestionTemplate, menuState : MenuState }

type Msg = UpdateModel (Model -> Model)
         | ChangedUrl Url
         | ClickedLink Browser.UrlRequest
         | MenuAction MenuMsg
         | QuestionTemplateLoaded QuestionTemplate
         | QuestionTemplateSaved
         | NoAction

type SelectedEntity a = Question (Focus a Question)
                      | Option (Focus a QuestionOption)
                      | Category (Focus a QuestionCategory )
                      | None

-- Id generation

type alias Seedable a = { a | uuidSeed : Seed }

withNewId : (String -> Seedable a -> Seedable a) -> Seedable a -> Seedable a
withNewId f model =
            let
                (newUuid, newSeed) =
                    step uuidGenerator model.uuidSeed
            in
                f (Uuid.toString newUuid) { model | uuidSeed = newSeed } 

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

-- Helpers

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
                            [] -> Question <| focus_
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
                            [] -> Question <| focus_
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
                            [] -> Question <| focus_
                            _  -> subQuestionRouteToSelectedEntity xs focus_
                
                _ -> None
        
    in        
        categoryRouteToSelectedEntity 

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
                        Question focus -> questionDetailsView model modelTraits focus
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
