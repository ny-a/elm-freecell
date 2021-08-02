module Main exposing (main)

import Browser
import Element exposing (Element, alignRight, centerY, column, el, fill, padding, rgb255, row, spacing, text, width)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import FreeCell exposing (..)
import Html exposing (Html)
import Html.Attributes
import Task
import Random


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
    , newGameNum : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    Model (initializeDeck initialNewGameNum) Nothing initialCardFoundation initialNewGameNum
        |> withStartRandomNewGame


initialNewGameNum : Int
initialNewGameNum =
    1


withNoneCmd : Model -> ( Model, Cmd msg )
withNoneCmd model =
    Tuple.pair model Cmd.none


withCheckingFoundationMovability : Model -> ( Model, Cmd Msg )
withCheckingFoundationMovability model =
    Task.perform CheckFoundationMovability (Task.succeed <| findMovableCardToFoundation model.cardFoundation model.deckTable)
        |> Tuple.pair model


withStartRandomNewGame : Model -> ( Model, Cmd Msg )
withStartRandomNewGame model =
    Random.generate StartNewGameWithGameNumber (Random.int 1 1000000)
        |> Tuple.pair model



-- UPDATE


type Msg
    = OnClickCol Int
    | MoveCardToFoundation
    | CheckFoundationMovability (Maybe Int)
    | UpdateNewGameNum String
    | StartNewGame
    | StartNewGameWithGameNumber Int
    | StartRandomGame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnClickCol col ->
            let
                newModel =
                    case model.selectedCol of
                        Just beforeCol ->
                            { model
                            | deckTable = moveCardWithCheck beforeCol col model.deckTable,
                            selectedCol = Nothing
                            }
                                |> withCheckingFoundationMovability

                        Nothing ->
                            let
                                selectedCol =
                                    if model.selectedCol /= Just col && checkCardExistent col model.deckTable then
                                        Just col

                                    else
                                        Nothing
                            in
                            { model | selectedCol = selectedCol }
                                |> withNoneCmd
            in
            newModel

        MoveCardToFoundation ->
            let
                ( foundation, deckTable ) =
                    case model.selectedCol of
                        Just card ->
                            moveCardToFoundation card model.cardFoundation model.deckTable

                        Nothing ->
                            ( model.cardFoundation, model.deckTable )
            in
            { model
            | cardFoundation = foundation
            , deckTable = deckTable
            , selectedCol = Nothing
            }
                |> withCheckingFoundationMovability

        CheckFoundationMovability maybeCol ->
            case maybeCol of
                Just _ ->
                    update MoveCardToFoundation
                        { model | selectedCol = maybeCol }

                Nothing ->
                    model |> withNoneCmd

        UpdateNewGameNum newGameNum ->
            { model
            | newGameNum = Maybe.withDefault 0 <| String.toInt newGameNum
            }
                |> withNoneCmd

        StartNewGame ->
            { model
            | deckTable = initializeDeck model.newGameNum
            , selectedCol = Nothing
            , cardFoundation = initialCardFoundation
            }
                |> withNoneCmd

        StartNewGameWithGameNumber newGameNum ->
            { model
            | newGameNum = newGameNum
            }
                |> update StartNewGame
                |> Tuple.first
                |> withNoneCmd

        StartRandomGame ->
            withStartRandomNewGame model


-- VIEW


view : Model -> Html Msg
view model =
    Element.layout [] <|
        viewOfFreecellApp model


gray : Element.Color
gray = rgb255 238 238 238


viewOfFreecellApp : Model -> Element Msg
viewOfFreecellApp model =
    column [ spacing 20 ]
        [ Element.html <|
            Html.header [ Html.Attributes.class "site-header" ]
                [ Html.h1 [] [ Html.text "Freecell" ] ]
        , row [ spacing 20 ]
            [ Element.Input.text []
                { onChange = UpdateNewGameNum
                , text = String.fromInt model.newGameNum
                , placeholder = Nothing
                , label = Element.Input.labelLeft [] <| text "NewGameNumber: "
                }
            , Element.Input.button
                [ Element.padding 5
                , Element.Border.rounded 5
                , Element.Background.color gray
                ]
                { onPress = Just StartNewGame, label = text "StartNewGame" }
            , Element.Input.button
                [ Element.padding 5
                , Element.Border.rounded 5
                , Element.Background.color gray
                ]
                { onPress = Just StartRandomGame
                , label = text "StartRandomGame"
                }
            ]
        , viewOfFreecellDeck model
        ]


viewOfFreecellDeck : Model -> Element Msg
viewOfFreecellDeck model =
    column [ spacing 20 ]
        [ row [ spacing 30 ]
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
            [ el [ Element.htmlAttribute <| Html.Attributes.class "card-empty" ] (text "") ]

        _ ->
            list


viewOfFreecellDeckColumn : Maybe Int -> Int -> List Int -> Element Msg
viewOfFreecellDeckColumn selectedCol col list =
    let
        conv item =
            el
                [ cardFontColor item, cardIsClicked selectedCol col, Element.htmlAttribute <| Html.Attributes.class "cards" ]
                (text <| numberToCardStr item)
    in
    column [ Element.alignTop, cardOnClick col ] <|
        viewOfFreecellDeckEmptyColumn <|
            List.map conv <|
                List.reverse list


viewsOfFreecellRow : Maybe Int -> List (Maybe Int) -> List (Element Msg)
viewsOfFreecellRow selectedCol listf =
    listf
        |> List.indexedMap (viewOfFreecellCards selectedCol)
        |> List.map (el [])


viewOfFreecellFoundationRows : CardFoundation -> List (Element Msg)
viewOfFreecellFoundationRows foundation =
    cardFoundationToList foundation
        |> List.indexedMap
            (\suit origNum ->
                let
                    num =
                        origNum * 4 + suit
                in
                column [ Element.htmlAttribute <| Html.Attributes.class "foundation-cards" ]
                    (if origNum == -1 then
                        [ el [ Element.htmlAttribute <| Html.Attributes.class "card-foundation-background", Element.centerX, cardFontColor suit ] (text <| numberToSuites suit)
                        , el [ Element.htmlAttribute <| Html.Attributes.class "card-foundation-empty" ] (text "")
                        ]

                     else
                        [ el [ cardFontColor num, Element.htmlAttribute <| Html.Attributes.class "cards-foundation" ] (text <| numberToCardStr num) ]
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
                , Element.htmlAttribute <| Html.Attributes.class "cards"
                ]
                (text <| numberToCardStr num)

        Nothing ->
            el
                [ cardIsClicked selectedCol idx
                , cardOnClick idx
                , Element.htmlAttribute <| Html.Attributes.class "card-empty"
                ]
                (text "")


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
        Element.htmlAttribute <| Html.Attributes.class "cards-selected"

    else
        Element.Font.unitalicized


cardOnClick : Int -> Element.Attribute Msg
cardOnClick col =
    Element.Events.onClick <| OnClickCol col
