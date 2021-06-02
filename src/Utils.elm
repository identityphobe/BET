module Utils exposing (..)


find : (a -> Bool) -> List a -> Maybe a
find predicate list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            if predicate first then
                Just first

            else
                find predicate rest



-- Finds and update if any
-- It returns the same List if there's nothing to update


findAndUpdate : (a -> Bool) -> (a -> a) -> List a -> List a
findAndUpdate predicate update list =
    case list of
        [] ->
            []

        first :: rest ->
            if predicate first then
                update first :: rest

            else
                first :: findAndUpdate predicate update rest
