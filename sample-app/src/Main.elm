module Main exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Navigation
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser)


type alias Model =
    { key : Navigation.Key
    , route : Route
    }


type Route
    = Index
    | NotFound


parseRoute : Url -> Route
parseRoute url =
    url
        |> Parser.parse (Parser.oneOf [ Parser.map Index Parser.top ])
        |> Maybe.withDefault NotFound


pathFor : Route -> String
pathFor route =
    case route of
        Index ->
            "/"

        NotFound ->
            "/404"


type Msg
    = UrlChanged Url
    | UrlRequested UrlRequest


type alias Flags =
    ()


init : Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { key = key
      , route = parseRoute url
      }
    , Cmd.none
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


view : Model -> Browser.Document Msg
view model =
    { title = "Datalog Sample Apps"
    , body =
        [ Html.div
            []
            [ Html.header []
                [ Html.nav []
                    [ Html.ol
                        []
                        [ Html.li [] [ Html.a [ Attrs.href (pathFor Index) ] [ Html.text "Index" ] ] ]
                    ]
                ]
            , Html.main_ []
                [ case model.route of
                    Index ->
                        viewIndex model

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
