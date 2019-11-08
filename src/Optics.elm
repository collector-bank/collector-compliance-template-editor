module Optics exposing (..)

import Monocle.Optional exposing (Optional)
import Monocle.Compose as Compose

type alias Focus a b = { optional : Optional a b, path : List String }

-- pipeline friendly compose: f1 |> composeFocus f2 
composeFocus : Focus b c -> Focus a b -> Focus a c
composeFocus f2 f1 = { optional = f1.optional |> Compose.optionalWithOptional f2.optional, path = f1.path ++ f2.path }

elementOfElementList : (a -> Bool) -> Optional (List a) a
elementOfElementList predicate = 
    let 
        tryFind xs = List.filter predicate xs |> List.head
        addOrReplace x_ xss = 
            case xss of
                [] -> [x_]
                x::xs -> if predicate x then x_ :: xs else x :: addOrReplace x_ xs
    in
        Optional tryFind addOrReplace

