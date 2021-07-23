module Main exposing (main)

import Browser
import Element exposing (Element, alignRight, centerY, column, el, fill, padding, rgb255, row, spacing, text, width)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import FreeCell exposing (..)
import Html exposing (Html)
import Html.Attributes


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { deckTable : DeckTable
    , selectedCol : Maybe Int
    , cardFoundation : CardFoundation
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model (initializeDeck 1) Nothing initialCardFoundation
    , Cmd.none
    )


withNoneCmd : Model -> ( Model, Cmd msg )
withNoneCmd model =
    Tuple.pair model Cmd.none



-- UPDATE


type Msg
    = OnClickCol Int
    | MoveCard Int Int
    | MoveCardToFoundation


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnClickCol col ->
            let
                selectedCol =
                    if model.selectedCol == Nothing && model.selectedCol /= Just col && checkCardExistent col model.deckTable then
                        Just col

                    else
                        Nothing

                newModel =
                    case model.selectedCol of
                        Just beforeCol ->
                            model
                                |> update (MoveCard beforeCol col)
                                |> Tuple.first

                        Nothing ->
                            model
            in
            { newModel | selectedCol = selectedCol }
                |> withNoneCmd

        MoveCard fromCol toCol ->
            let
                deckTable =
                    moveCardWithCheck fromCol toCol model.deckTable
            in
            { model | deckTable = deckTable }
                |> withNoneCmd

        MoveCardToFoundation ->
            let
                ( foundation, deckTable ) =
                    case model.selectedCol of
                        Just card ->
                            moveCardToFoundation card model.cardFoundation model.deckTable

                        Nothing ->
                            ( model.cardFoundation, model.deckTable )
            in
            { model | cardFoundation = foundation, deckTable = deckTable, selectedCol = Nothing }
                |> withNoneCmd



-- VIEW


view : Model -> Html Msg
view model =
    Element.layout [] <|
        viewOfFreecellDeck model


viewOfFreecellDeck : Model -> Element Msg
viewOfFreecellDeck model =
    column [ spacing 20 ]
        [ row [ spacing 60 ]
            [ row [ spacing 30 ] <|
                viewsOfFreecellRow model.selectedCol <|
                    convertDeckTableFreecellToList model.deckTable
            , row [ spacing 30, Element.Events.onClick MoveCardToFoundation ] <|
                viewOfFreecellFoundationRows model.cardFoundation
            ]
        , row [ width fill, spacing 30 ] <|
            List.indexedMap (viewOfFreecellDeckColumn model.selectedCol) <|
                convertDeckTableToList model.deckTable
        ]


viewOfFreecellDeckEmptyColumn : List (Element Msg) -> List (Element Msg)
viewOfFreecellDeckEmptyColumn list =
    case list of
        [] ->
            [ el [ Element.Font.underline ] (text "  ") ]

        _ ->
            list


viewOfFreecellDeckColumn : Maybe Int -> Int -> List Int -> Element Msg
viewOfFreecellDeckColumn selectedCol col list =
    let
        conv item =
            el
                [ cardFontColor item, cardIsClicked selectedCol col ]
                (text <| numberToCardStr item)
    in
    column [ Element.alignTop, spacing 10, cardOnClick col ] <|
        viewOfFreecellDeckEmptyColumn <|
            List.map conv <|
                List.reverse list


viewsOfFreecellRow : Maybe Int -> List Int -> List (Element Msg)
viewsOfFreecellRow selectedCol listf =
    List.append (List.map Just listf) (List.repeat 4 Nothing)
        |> List.take 4
        |> List.indexedMap (viewOfFreecellCards selectedCol)


viewOfFreecellFoundationRows : CardFoundation -> List (Element Msg)
viewOfFreecellFoundationRows foundation =
    cardFoundationToList foundation
        |> List.indexedMap
            (\suit num ->
                el []
                    (if num == -1 then
                        el [ Element.Font.underline ] (text "  ")

                     else
                        text <| numberToCardStr <| num * 4 + suit
                    )
            )


viewOfFreecellCards : Maybe Int -> Int -> Maybe Int -> Element Msg
viewOfFreecellCards selectedCol origIdx maybeNum =
    let
        idx =
            origIdx + 8
    in
    case maybeNum of
        Just num ->
            el
                [ cardFontColor num
                , cardIsClicked selectedCol idx
                , cardOnClick idx
                ]
                (text <| numberToCardStr num)

        Nothing ->
            el
                [ Element.Font.underline
                , cardIsClicked selectedCol idx
                , cardOnClick idx
                ]
                (text "  ")


cardFontColor : Int -> Element.Attribute msg
cardFontColor num =
    Element.Font.color
        (if numberToSuitColorIsRed num then
            rgb255 255 0 0

         else
            rgb255 0 0 0
        )


cardIsClicked : Maybe Int -> Int -> Element.Attribute Msg
cardIsClicked selectedCol col =
    if selectedCol == Just col then
        Element.Font.underline

    else
        Element.Font.unitalicized


cardOnClick : Int -> Element.Attribute Msg
cardOnClick col =
    Element.Events.onClick <| OnClickCol col
