module Route exposing (Route(..), fromLocation)

import Navigation exposing (Location)
import UrlParser exposing (Parser, parseHash, s)


type Route
    = Index
    | Ninja


route : Parser (Route -> a) a
route =
    UrlParser.map Ninja (s "ninja")


fromLocation : Location -> Route
fromLocation location =
    parseHash route location |> Maybe.withDefault Index
