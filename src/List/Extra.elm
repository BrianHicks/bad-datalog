module List.Extra exposing (foldrResult, indexOf)


foldrResult : (a -> b -> Result x b) -> b -> List a -> Result x b
foldrResult fn initial items =
    foldrResultHelp fn initial (List.reverse items)


foldrResultHelp : (a -> b -> Result x b) -> b -> List a -> Result x b
foldrResultHelp fn soFar items =
    case items of
        [] ->
            Ok soFar

        next :: rest ->
            case fn next soFar of
                Ok updated ->
                    foldrResultHelp fn updated rest

                Err problem ->
                    Err problem


indexOf : a -> List a -> Maybe Int
indexOf =
    indexOfHelp 0


indexOfHelp : Int -> a -> List a -> Maybe Int
indexOfHelp idx item items =
    case items of
        [] ->
            Nothing

        first :: rest ->
            if first == item then
                Just idx

            else
                indexOfHelp (idx + 1) item rest
