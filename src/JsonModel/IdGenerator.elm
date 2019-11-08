module JsonModel.IdGenerator exposing (generateIds)

import QuestionCategory.Model exposing(..)
import Question.Model exposing (..)
import QuestionTemplate.Model exposing (..)

import State exposing (State, state, andThen, get, modify, embed, traverse)
import Uuid exposing (Uuid, uuidGenerator)
import Random exposing (Seed, step, initialSeed)
import Question.Model exposing (..)

type alias Rnd = { seed : Seed, id : String }

type alias GeneratorState a = State Rnd a

newId : GeneratorState String
newId = 
    let 
        generate rnd = 
            let 
                (newUuid, newSeed) = step uuidGenerator rnd.seed
            in
                { seed = newSeed, id = Uuid.toString newUuid }
    in
        modify generate |> andThen (\_ -> get) |> State.map .id

pickId : String -> String -> String
pickId generated existing = if String.isEmpty existing then generated else existing

generateIds : QuestionTemplate -> QuestionTemplate
generateIds questionTemplate = State.finalValue { seed = initialSeed 0, id = "" } (generateIdsForQuestionTemplate questionTemplate)

generateIdsForQuestionTemplate : QuestionTemplate -> GeneratorState QuestionTemplate
generateIdsForQuestionTemplate questionTemplate = 
    generateIdsForQuestionCategories questionTemplate.categories |>
    andThen (\categories -> state { questionTemplate | categories = categories  })

generateIdsForQuestionCategories : List QuestionCategory -> GeneratorState (List QuestionCategory)
generateIdsForQuestionCategories questionCategories = traverse generateIdsForQuestionCategory questionCategories

generateIdsForQuestionCategory : QuestionCategory -> GeneratorState QuestionCategory
generateIdsForQuestionCategory questionCategory = 
    generateIdsForQuestions questionCategory.questions |>
    andThen (\questions -> state { questionCategory | questions = questions } )

generateIdsForQuestions : List Question -> GeneratorState (List Question)
generateIdsForQuestions questions = traverse generateIdsForQuestion questions

generateIdsForQuestion : Question -> GeneratorState Question
generateIdsForQuestion (Question q) = 
    let         
        makeQuestion id =
            generateIdsForQuestionType q.questionType |>
            andThen (\qt -> state (Question { q | id = pickId id q.id, questionType = qt }))
    in
        newId |> andThen makeQuestion


generateIdsForQuestionType : QuestionType -> GeneratorState QuestionType
generateIdsForQuestionType questionType =
    case questionType of
        QuestionGroup qg -> 
            generateIdsForQuestions qg.questions |>
            andThen (\questions -> state <| QuestionGroup { qg | questions = questions })           
        SelectQuestion sq ->
            generateIdsForOptions sq.options |>
            andThen (\options -> state <| SelectQuestion { sq | options = options })
        _ -> state questionType

generateIdsForOptions : List QuestionOption -> GeneratorState (List QuestionOption)
generateIdsForOptions options = traverse generateIdsForOption options

generateIdsForOption : QuestionOption -> GeneratorState QuestionOption
generateIdsForOption option = 
    newId |> andThen
        (\id -> 
            generateIdsForQuestions option.followUpQuestions |>
            andThen (\followUpQuestions -> state { option | id = pickId id option.id, followUpQuestions = followUpQuestions }))


