module Main exposing (..)

import Browser
import Browser.Navigation exposing (Key)
import Css
import Css.Global
import Datalog
import Datalog.Atom as Atom
import Datalog.Parser
import Dict
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events as Events
import Url exposing (Url)


type Evaluation
    = Blank
    | Error (List String)
    | Unsolved Datalog.Program
    | Solved Datalog.Database


type alias Model =
    { source : String
    , evaluation : Evaluation
    , autoSolve : Bool
    , key : Key
    }


type Msg
    = NewSource String
    | SetAutoSolve Bool
    | Solve
    | OnUrlChange Url
    | OnUrlRequest Browser.UrlRequest


init : () -> Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    ( { source = ""
      , evaluation = Blank
      , autoSolve = True
      , key = key
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewSource source ->
            ( { model
                | source = source
                , evaluation =
                    case Datalog.Parser.parse source of
                        Err problems ->
                            Error problems

                        Ok program ->
                            if model.autoSolve then
                                Solved (Datalog.solve program)

                            else
                                Unsolved program
              }
            , Cmd.none
            )

        SetAutoSolve True ->
            ( { model
                | autoSolve = True
                , evaluation =
                    case model.evaluation of
                        Unsolved program ->
                            Solved (Datalog.solve program)

                        _ ->
                            model.evaluation
              }
            , Cmd.none
            )

        SetAutoSolve False ->
            ( { model | autoSolve = False }, Cmd.none )

        Solve ->
            ( { model
                | evaluation =
                    case model.evaluation of
                        Unsolved program ->
                            Solved (Datalog.solve program)

                        _ ->
                            model.evaluation
              }
            , Cmd.none
            )

        OnUrlChange url ->
            ( model, Cmd.none )

        OnUrlRequest requst ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Datalog Time!"
    , body =
        [ Css.Global.global [ Css.Global.html [ Css.backgroundColor (Css.hex "B0E0E6") ] ]
            |> Html.toUnstyled
        , Html.main_
            [ css
                [ Css.maxWidth (Css.px 800)
                , Css.minHeight (Css.vh 100)
                , Css.margin2 Css.zero Css.auto
                , Css.padding (Css.px 20)
                , Css.boxSizing Css.borderBox
                , Css.boxShadow5 Css.zero Css.zero (Css.px 10) (Css.px 1) (Css.rgba 0 0 0 0.25)
                , Css.fontFamily Css.sansSerif
                , Css.backgroundColor (Css.hex "FFF")
                ]
            ]
            [ Html.h1 []
                [ Html.text "Datalog Time! ("
                , Html.a [ Attributes.href "https://git.bytes.zone/brian/bad-datalog" ] [ Html.text "source" ]
                , Html.text ")"
                ]
            , Html.label []
                [ Html.input
                    [ Attributes.type_ "checkbox"
                    , Attributes.checked model.autoSolve
                    , Events.onCheck SetAutoSolve
                    ]
                    []
                , Html.text "automatically solve"
                ]
            , Html.textarea
                [ Events.onInput NewSource
                , Attributes.value model.source
                , css
                    [ Css.width (Css.pct 100)
                    , Css.height (Css.px 300)
                    , Css.padding (Css.px 20)
                    , Css.boxSizing Css.borderBox
                    , Css.border3 (Css.px 1) Css.solid (Css.hex "AAA")
                    , Css.borderRadius (Css.px 10)
                    , Css.fontFamily Css.monospace
                    ]
                ]
                []
            , case model.evaluation of
                Blank ->
                    Html.p [] [ Html.text "Enter a program above!" ]

                Error errors ->
                    Html.ul []
                        (List.map
                            (\err -> Html.li [] [ Html.pre [] [ Html.text err ] ])
                            errors
                        )

                Unsolved program ->
                    Html.p []
                        [ Html.text "Sweet, it parses! Now "
                        , Html.button [ Events.onClick Solve ] [ Html.text "solve" ]
                        , Html.text " this bad boy!"
                        ]

                Solved database ->
                    Html.dl []
                        (Dict.foldr
                            (\( name, _ ) ( first, rest ) soFar ->
                                Html.dt [] [ Html.pre [] [ Html.text name ] ]
                                    :: List.map
                                        (\atom -> Html.dd [] [ Html.pre [] [ Html.text (Atom.toString atom) ] ])
                                        (first :: rest)
                                    ++ soFar
                            )
                            []
                            database
                        )
            ]
            |> Html.toUnstyled
        ]
    }


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , onUrlChange = OnUrlChange
        , onUrlRequest = OnUrlRequest
        , subscriptions = \_ -> Sub.none
        }
