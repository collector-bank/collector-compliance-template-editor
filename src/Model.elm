module Model exposing (..)

import Browser exposing (Document,application)
import Browser.Navigation as Nav
import Monocle.Lens exposing (Lens)
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
import Url exposing (Url)
import Menu exposing (..)

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

type RoutePart
    = CategoryRoute String
    | QuestionRoute String
    | OptionsRoute String
    | GroupQuestion String
    | FollowUpQuestion String

type alias Route = List RoutePart

-- Id generation

type alias Seedable a = { a | uuidSeed : Seed }

withNewId : (String -> Seedable a -> Seedable a) -> Seedable a -> Seedable a
withNewId f model =
            let
                (newUuid, newSeed) =
                    step uuidGenerator model.uuidSeed
            in
                f (Uuid.toString newUuid) { model | uuidSeed = newSeed } 
