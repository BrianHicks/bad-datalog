module List.Extra exposing (foldrResult)


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
