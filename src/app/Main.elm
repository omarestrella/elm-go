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
    | Click Int Int


type Intersection
    = Black
    | White
    | Empty


type Player
    = First
    | Second


type alias Row =
    Array Intersection


type alias Board =
    Array Row


type alias Model =
    { board : Board
    , currentPlayer : Player
    }


size : Board -> Int
size board =
    Array.length board



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Click row col ->
            let
                nextPlayer =
                    case model.currentPlayer of
                        First ->
                            Second

                        Second ->
                            First

                intersectionValue =
                    case model.currentPlayer of
                        First ->
                            Black

                        Second ->
                            White

                currentRow =
                    Array.get row model.board |> Maybe.withDefault (Array.fromList [])

                newRow =
                    Array.set col intersectionValue currentRow

                newBoard =
                    Array.set row newRow model.board
            in
            ( { model | board = newBoard, currentPlayer = nextPlayer }, Cmd.none )



-- Views


intersection : Int -> ( Int, Intersection ) -> Html Msg
intersection row ( col, state ) =
    div
        [ class "intersection"
        , attribute "data-row" (String.fromInt row)
        , attribute "data-col" (String.fromInt col)
        ]
        [ case state of
            Black ->
                div [ class "piece black" ] []

            White ->
                div [ class "piece white" ] []

            Empty ->
                div [ class "piece empty", onClick (Click row col) ] []
        ]


rowView : Board -> Int -> Html Msg
rowView board rowPos =
    let
        size_ =
            size board

        maybeRow =
            Array.get rowPos board
    in
    case maybeRow of
        Just row ->
            div
                [ class "row" ]
                (List.map
                    (intersection rowPos)
                    (Array.toIndexedList row)
                )

        Nothing ->
            text ""


boardView : Board -> Html Msg
boardView board =
    let
        size_ =
            size board
    in
    div
        [ class "board" ]
        (List.map
            (rowView board)
            (List.range 1 size_)
        )


mainView : Model -> Html Msg
mainView model =
    div [ class "main" ]
        [ boardView model.board ]


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
    ( { board = emptyBoard 9, currentPlayer = First }
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
