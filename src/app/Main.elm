module Main exposing (main)

import Array exposing (Array)
import Browser exposing (Document, UrlRequest(..))
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (attribute, class, style)
import Html.Events exposing (onClick)
import Json.Encode as Encode



-- Models


type Msg
    = NoOp


type Intersection
    = Black
    | White
    | Empty


type alias Board =
    Array (Array Intersection)


type alias Model =
    { board : Board }


size : Board -> Int
size board =
    Array.length board



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- Views


intersection row col =
    div
        [ class "intersection"
        , attribute "data-row" (String.fromInt row)
        , attribute "data-col" (String.fromInt col)
        ]
        []


boardView size_ =
    div
        [ class "board" ]
        (List.map
            (\currentRow ->
                div
                    [ class "row"
                    ]
                    (List.map
                        (\currentCol ->
                            intersection currentRow currentCol
                        )
                        (List.range 0 (size_ - 1))
                    )
            )
            (List.range 0 (size_ - 1))
        )


mainView : Model -> Html Msg
mainView model =
    let
        size_ =
            size model.board
    in
    div [ class "main" ]
        [ boardView size_ ]


view : Model -> Browser.Document Msg
view model =
    { title = "Elm App"
    , body = [ mainView model ]
    }



-- Main


emptyBoard : Int -> Board
emptyBoard size_ =
    Array.initialize size_
        (\_ -> Array.initialize size_ (always Empty))


init : String -> ( Model, Cmd Msg )
init _ =
    ( { board = emptyBoard 9 }
    , Cmd.none
    )


main : Program String Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
