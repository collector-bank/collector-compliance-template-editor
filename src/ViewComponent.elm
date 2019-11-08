module ViewComponent exposing (..)

import Optics exposing (Focus)
import Html exposing (Html)

-- View component types

-- am = app model type
-- msg = the message type
type alias ModelTraits am msg = 
    {
        makeMsg : (am -> am) -> msg,
        makeIdMsg : (String -> am -> am) -> msg
    }

-- cm  = the component model type
-- am  = the app model type (leave generic)
-- msg = the message type (leave generic)
type alias ViewComponent cm am msg = am -> ModelTraits am msg -> Focus am cm -> Html msg

-- Kind of a hack but at least keeps this ugly code in one place
eval : (am -> ModelTraits am msg -> Focus am cm -> cm -> Html msg) -> ViewComponent cm am msg
eval f model modelTraits focus =
    case focus.optional.getOption model of 
        Just model_ -> f model modelTraits focus model_
        Nothing -> Html.text ""
