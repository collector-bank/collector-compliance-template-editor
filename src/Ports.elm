port module Ports exposing(..)

port loadQuestionTemplate : String -> Cmd msg
port questionTemplateLoaded : (String -> msg) -> Sub msg
port saveQuestionTemplate : (String, String) -> Cmd msg
port questionTemplateSaved : (String -> msg) -> Sub msg
port productsLoaded : (String -> msg) -> Sub msg

