module Question.Model exposing (..)

type alias QuestionFields =
    { id : String
    , title : String
    , extendedTitle : String
    , description : String
    , extendedDescription : String
    , parameters : List (String,String)
    , questionType : QuestionType
    , active : Bool    
    , collapsed : Bool } 

type Question = Question QuestionFields

type alias QuestionGroupFields = { questions : List Question, maxRepeat : String }
type alias FreeTextQuestionFields = { maxLength : String, isMandatory : Bool }
type alias CountryQuestionFields = { allowMultiple: Bool, validationMessage : String }
type alias SelectQuestionFields = { options : List QuestionOption, allowMultiple: Bool }

type QuestionType 
    = FreeTextQuestion FreeTextQuestionFields
    | SelectQuestion SelectQuestionFields
    | CountryQuestion  CountryQuestionFields
    | QuestionGroup QuestionGroupFields

type alias QuestionOption = 
    { id : String
    , text : String
    , isComplianceCheckNeeded : Bool
    , followUpQuestions : List Question
    , active : Bool 
    }

makeQuestion : String -> Question
makeQuestion id = 
    Question 
    { id = id
    , title = id
    , extendedTitle = ""
    , description = ""
    , extendedDescription = ""
    , parameters = []
    , questionType = makeFreeTextQuestion
    , active = False
    , collapsed = False }

makeOption : String -> QuestionOption
makeOption id = 
    { id = id
    , text = id
    , isComplianceCheckNeeded = False
    , followUpQuestions = []
    , active = False }

makeFreeTextQuestion : QuestionType
makeFreeTextQuestion = FreeTextQuestion { maxLength = "", isMandatory = False }

makeQuestionGroup : QuestionType
makeQuestionGroup = QuestionGroup { questions = [], maxRepeat = "1" }

makeSelectQuestion : QuestionType
makeSelectQuestion = SelectQuestion  { options = [], allowMultiple = False }

makeCountryQuestion : QuestionType
makeCountryQuestion = CountryQuestion  { allowMultiple = False, validationMessage = "" }

isFreeText questionType =
    case questionType of 
        FreeTextQuestion _ -> True
        _ -> False

isSelectQuestion questionType = 
    case questionType of 
        SelectQuestion _ -> True
        _ -> False

isCountryQuestion questionType =
    case questionType of 
        CountryQuestion _ -> True
        _ -> False

isQuestionGroup questionType = 
    case questionType of 
        QuestionGroup _ -> True
        _ -> False
