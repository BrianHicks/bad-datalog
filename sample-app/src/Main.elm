module Main exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Navigation
import Css
import Css.Global as Global
import FamilyTree
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs exposing (css)
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
        Html.div
            [ css
                [ Css.maxWidth (Css.px 1024)
                , Css.minHeight (Css.vh 100)
                , Css.margin2 Css.zero Css.auto
                , Css.padding (Css.px 25)
                , Css.boxShadow4 (Css.px 0) (Css.px 0) (Css.px 80) (Css.hex "B2EBF2")
                , Css.backgroundColor (Css.hex "FFF")
                ]
            ]
            [ Global.global
                [ Global.body
                    [ Css.margin Css.zero
                    , Css.fontFamily Css.sansSerif
                    , Css.backgroundColor (Css.hex "E0F7FA")
                    , Css.color (Css.hex "37474F")
                    ]
                , Global.everything [ Css.boxSizing Css.borderBox ]
                ]
            , Html.header
                [ css
                    [ Css.displayFlex
                    , Css.marginBottom (Css.px 25)
                    ]
                ]
                [ Html.p
                    [ css
                        [ Css.marginRight (Css.px 10)
                        , Css.fontWeight Css.bold
                        ]
                    ]
                    [ Html.text "Datalog Sample Apps" ]
                , Html.nav []
                    [ Html.ol
                        [ css
                            [ Css.listStyleType Css.none
                            , Css.margin Css.zero
                            , Css.padding Css.zero
                            , Css.displayFlex
                            ]
                        ]
                        [ viewHeaderLink Index "Index"
                        , viewHeaderLink FamilyTree "Family Tree"
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
            |> List.singleton
    }


viewHeaderLink : Route -> String -> Html msg
viewHeaderLink route caption =
    Html.li
        [ css
            [ Css.paddingRight (Css.px 10)
            ]
        ]
        [ Html.p []
            [ Html.a
                [ Attrs.href (pathFor route) ]
                [ Html.text caption ]
            ]
        ]


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
