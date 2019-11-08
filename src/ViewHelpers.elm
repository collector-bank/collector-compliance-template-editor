module ViewHelpers exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Color exposing (Color)
import Json.Decode as Json
--import Svg exposing (Svg)

treeViewButtons : List (String, msg) -> { m | active : Bool } -> Html msg
treeViewButtons buttons model =
    let 
        makeButton : (String, u) -> Html u
        makeButton (title, action) =
            button [ class "btn btn-outline-primary btn-sm", onClick action ]
                [ text title ]
    in
    span [ class "float-right"
         , style "display" (if model.active then "" else "none")
         ] 
         (List.map makeButton buttons)

formTextInput : String -> String -> String -> Maybe String -> (String -> msg) -> Html msg
formTextInput id_ text_ value_ maybeHelp action =
    div [ class "form-group" ]
        [ label [ for id_ ]
            [ text text_ ]
        , input [ type_ "text", class "form-control", id id_, onInput action, value value_ ]
            []
        , case maybeHelp of
            Just helpText ->
                small [ id (id_ ++ "help"), class "form-text text-muted" ]
                    [ text helpText ]
            Nothing -> text ""
        ]

formCheckBoxInput : String -> String -> Bool -> msg -> Html msg
formCheckBoxInput id_ text_ checked_ toggleAction =
    div [ class "form-check" ]
        [ input [ type_ "checkbox", class "form-check-input", id id_, onClick toggleAction, checked checked_ ]
            []
        , label [ for id_, class "form-check-label" ]
            [ text text_ ]            
        ]

formSelectInput : String -> String -> (String -> msg) ->  List (String, String, Bool) -> Html msg
formSelectInput id_ text_ action options  =
    let
        makeOption (option_, value_, selected_) = option [ value value_, selected selected_ ] [ text option_]
    in        
        div [ class "form-group" ]
            [ label [ for id_ ]
                [ text text_ ]
            , select [ class "form-control", id id_, onInput action ]
              (List.map makeOption options)
            ]

treeIcon : (Color.Color -> Int -> Html msg) -> Bool -> Html msg
treeIcon icon active = span [ style "vertical-align" "sub", style "padding-right" "5px" ] [(icon (if active then Color.red else Color.black) 20)]

treeIcon2 : (Color.Color -> number -> Html a) -> (Color.Color -> number -> Html a) -> Bool -> Bool -> a -> Html a
treeIcon2 iconExpanded iconCollapsed active collapsed toggleCollapsed = 
    span [ style "vertical-align" "sub", style "padding-right" "5px", onClick toggleCollapsed] 
        [ (if collapsed then iconCollapsed else iconExpanded) (if active then Color.red else Color.black) 20
        ]

panel : String -> Html msg -> Html msg
panel heading content =
    div [ class "card", style "margin" "5px" ] 
        [ div [ class "card-body" ]
                [ h1 [ class "card-title", style "color" "#6b1faf"] [ text heading ]
                , div [ style "height" "50rem", style "overflow-y" "auto"] [content] 
                ]
        ]

panel2 : String -> Html msg -> Html msg
panel2 heading content =
    div [ class "card", style "margin" "5px" ] 
        [ div [ class "card-body" ]
                [ h1 [ class "card-title", style "color" "#6b1faf"] [ text heading ]
                , div [] [content] 
                ]
        ]

onChange : (String -> msg) -> Attribute msg
onChange msgCreator =
    Html.Events.on "change" (Json.map msgCreator Html.Events.targetValue)
