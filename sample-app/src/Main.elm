module Main exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Navigation
import FamilyTree
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser)


type alias Model =
    { key : Navigation.Key
    , route : Route
    , familyTree : FamilyTree.Model
    }


type Route
    = Index
    | FamilyTree
    | NotFound


parseRoute : Url -> Route
parseRoute url =
    url
        |> Parser.parse
            (Parser.oneOf
                [ Parser.map Index Parser.top
                , Parser.map FamilyTree (Parser.top </> Parser.s "family-tree")
                ]
            )
        |> Maybe.withDefault NotFound


pathFor : Route -> String
pathFor route =
    case route of
        Index ->
            "/"

        FamilyTree ->
            "/family-tree"

        NotFound ->
            "/404"


type Msg
    = UrlChanged Url
    | UrlRequested UrlRequest
    | FamilyTreeMsg FamilyTree.Msg


type alias Flags =
    ()


init : Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        ( familyTree, familyTreeCmd ) =
            FamilyTree.init
    in
    ( { key = key
      , route = parseRoute url
      , familyTree = familyTree
      }
    , Cmd.map FamilyTreeMsg familyTreeCmd
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChanged url ->
            ( { model | route = parseRoute url }
            , Cmd.none
            )

        UrlRequested (Internal url) ->
            ( model
            , Navigation.pushUrl model.key (Url.toString url)
            )

        UrlRequested (External url) ->
            ( model
            , Navigation.load url
            )

        FamilyTreeMsg familyTreeMsg ->
            let
                ( newFamilyTree, familyTreeCmd ) =
                    FamilyTree.update familyTreeMsg model.familyTree
            in
            ( { model | familyTree = newFamilyTree }
            , Cmd.map FamilyTreeMsg familyTreeCmd
            )


view : Model -> Browser.Document Msg
view model =
    { title =
        case model.route of
            Index ->
                "Datalog Sample Apps"

            FamilyTree ->
                "Address Book | Datalog Sample Apps"

            NotFound ->
                "Not Found | Datalog Sample Apps"
    , body =
        [ Html.div
            []
            [ Html.header []
                [ Html.nav []
                    [ Html.ol
                        []
                        [ Html.li [] [ Html.a [ Attrs.href (pathFor Index) ] [ Html.text "Index" ] ]
                        , Html.li [] [ Html.a [ Attrs.href (pathFor FamilyTree) ] [ Html.text "Family Tree" ] ]
                        ]
                    ]
                ]
            , Html.main_ []
                [ case model.route of
                    Index ->
                        viewIndex model

                    FamilyTree ->
                        FamilyTree.view model.familyTree
                            |> Html.map FamilyTreeMsg

                    NotFound ->
                        viewNotFound
                ]
            ]
            |> Html.toUnstyled
        ]
    }


viewIndex : Model -> Html Msg
viewIndex model =
    Html.text "Index!"


viewNotFound : Html Msg
viewNotFound =
    Html.text "Not Found!"


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlRequested
        }
