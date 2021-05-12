module Main exposing (..)

import AddressBook
import Browser exposing (UrlRequest(..))
import Browser.Navigation as Navigation
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser)


type alias Model =
    { key : Navigation.Key
    , route : Route
    , addressBook : AddressBook.Model
    }


type Route
    = Index
    | AddressBook
    | NotFound


parseRoute : Url -> Route
parseRoute url =
    url
        |> Parser.parse
            (Parser.oneOf
                [ Parser.map Index Parser.top
                , Parser.map AddressBook (Parser.top </> Parser.s "address-book")
                ]
            )
        |> Maybe.withDefault NotFound


pathFor : Route -> String
pathFor route =
    case route of
        Index ->
            "/"

        AddressBook ->
            "/address-book"

        NotFound ->
            "/404"


type Msg
    = UrlChanged Url
    | UrlRequested UrlRequest
    | AddressBookMsg AddressBook.Msg


type alias Flags =
    ()


init : Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        ( addressBook, addressBookCmd ) =
            AddressBook.init
    in
    ( { key = key
      , route = parseRoute url
      , addressBook = addressBook
      }
    , Cmd.map AddressBookMsg addressBookCmd
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

        AddressBookMsg addressBookMsg ->
            let
                ( newAddressBook, addressBookCmd ) =
                    AddressBook.update addressBookMsg model.addressBook
            in
            ( { model | addressBook = newAddressBook }
            , Cmd.map AddressBookMsg addressBookCmd
            )


view : Model -> Browser.Document Msg
view model =
    { title =
        case model.route of
            Index ->
                "Datalog Sample Apps"

            AddressBook ->
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
                        , Html.li [] [ Html.a [ Attrs.href (pathFor AddressBook) ] [ Html.text "Address Book" ] ]
                        ]
                    ]
                ]
            , Html.main_ []
                [ case model.route of
                    Index ->
                        viewIndex model

                    AddressBook ->
                        AddressBook.view model.addressBook
                            |> Html.map AddressBookMsg

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
