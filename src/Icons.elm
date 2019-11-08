module Icons exposing (check_circle, questionIconExpanded, questionIconCollapsed, categoryIcon)

import Color exposing (Color)
import FeatherIcons as FI
import Html exposing (Html)
import Html.Attributes exposing (style)

type alias IconDef msg = Color.Color -> Int -> Html msg

mkIcon : FI.Icon -> IconDef msg
mkIcon icon = \color size -> icon |> FI.toHtml [ style "color" (Color.toCssString color) ]

check_circle : IconDef msg
check_circle = mkIcon FI.checkCircle

questionIconExpanded :  IconDef msg
questionIconExpanded = mkIcon FI.helpCircle

questionIconCollapsed :  IconDef msg
questionIconCollapsed = FI.helpCircle |> FI.withSize 16 |> mkIcon 

categoryIcon :  IconDef msg
categoryIcon = mkIcon FI.aperture
