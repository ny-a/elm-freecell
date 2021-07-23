module FreeCell exposing (..)

import Array exposing (Array)
import Bitwise


type CardFoundation
    = CardFoundation Int Int Int Int


type DeckTable
    = DeckTable (List Int) (List Int) (List Int) (List Int) (List Int) (List Int) (List Int) (List Int) (Maybe Int) (Maybe Int) (Maybe Int) (Maybe Int)


findMovableCardToFoundation : CardFoundation -> DeckTable -> Maybe Int
findMovableCardToFoundation foundation table =
    convertAllHeadOfDeckTableToList table
        |> List.indexedMap
            (\idx head ->
                head
                    |> Maybe.andThen
                        (\card ->
                            if checkCardFoundationAcceptance card foundation then
                                Just idx

                            else
                                Nothing
                        )
            )
        |> List.filterMap identity
        |> List.head


moveCardToFoundation : Int -> CardFoundation -> DeckTable -> ( CardFoundation, DeckTable )
moveCardToFoundation fromCol foundation table =
    let
        ( maybeCard, newTable ) =
            getCardFromTable fromCol table

        moved =
            case maybeCard of
                Just card ->
                    if checkCardFoundationAcceptance card foundation then
                        ( addCardToFoundation card foundation, newTable )

                    else
                        ( foundation, table )

                Nothing ->
                    ( foundation, table )
    in
    moved


addCardToFoundation : Int -> CardFoundation -> CardFoundation
addCardToFoundation card foundation =
    let
        suit =
            modBy 4 card

        num =
            card // 4

        (CardFoundation suit0 suit1 suit2 suit3) =
            foundation
    in
    case suit of
        0 ->
            CardFoundation num suit1 suit2 suit3

        1 ->
            CardFoundation suit0 num suit2 suit3

        2 ->
            CardFoundation suit0 suit1 num suit3

        _ ->
            CardFoundation suit0 suit1 suit2 num


checkCardFoundationAcceptance : Int -> CardFoundation -> Bool
checkCardFoundationAcceptance card foundation =
    let
        suit =
            modBy 4 card

        num =
            card // 4

        foundationNum =
            case suit of
                0 ->
                    let
                        (CardFoundation v _ _ _) =
                            foundation
                    in
                    v

                1 ->
                    let
                        (CardFoundation _ v _ _) =
                            foundation
                    in
                    v

                2 ->
                    let
                        (CardFoundation _ _ v _) =
                            foundation
                    in
                    v

                _ ->
                    let
                        (CardFoundation _ _ _ v) =
                            foundation
                    in
                    v
    in
    foundationNum + 1 == num


cardFoundationToList : CardFoundation -> List Int
cardFoundationToList (CardFoundation suit0 suit1 suit2 suit3) =
    [ suit0, suit1, suit2, suit3 ]


initialCardFoundation : CardFoundation
initialCardFoundation =
    CardFoundation -1 -1 -1 -1


freeDeckColumnCount : DeckTable -> Int
freeDeckColumnCount table =
    List.filterMap
        (\v ->
            if List.isEmpty v then
                Just ()

            else
                Nothing
        )
        (convertDeckTableToList table)
        |> List.length


freeFreecellCount : DeckTable -> Int
freeFreecellCount table =
    let
        listf =
            convertDeckTableFreecellToList table
                |> List.filterMap identity
    in
    4 - List.length listf


moveCardWithCheck : Int -> Int -> DeckTable -> DeckTable
moveCardWithCheck fromCol toCol table =
    if fromCol < 8 && toCol < 8 then
        moveSerialCardsWithFreeCell fromCol toCol table

    else
        moveFreecellCardWithCheck fromCol toCol table


moveSerialCardsWithFreeCell : Int -> Int -> DeckTable -> DeckTable
moveSerialCardsWithFreeCell fromCol toCol table =
    let
        fromCards =
            getCardsListFromTable fromCol table

        freeFreecells =
            freeFreecellCount table

        moveableCards =
            List.range freeFreecells (freeFreecells + freeDeckColumnCount table)
                |> List.sum
                |> (+) 1
    in
    fromCards
        |> List.foldl
            (\fromCard ( ( ( ( list, isPrevSerial ), idx ), max ), deckTable ) ->
                let
                    isSerial =
                        case List.head list of
                            Just prevCard ->
                                (idx < max)
                                    && isPrevSerial
                                    && (numberToSuitColorIsRed fromCard /= numberToSuitColorIsRed prevCard)
                                    && ((fromCard // 4) - 1 == prevCard // 4)

                            Nothing ->
                                True

                    isMovable =
                        isSerial
                            && checkCardMoveability fromCard toCol deckTable
                in
                if isMovable then
                    let
                        ( tookCards, tookTable ) =
                            takeCardsFromTable fromCol idx deckTable

                        newTable =
                            appendCardIntoTable tookCards toCol tookTable
                    in
                    ( ( ( ( fromCard :: list, False ), max ), max ), newTable )

                else
                    ( ( ( ( fromCard :: list, isSerial ), idx + 1 ), max ), deckTable )
            )
            ( ( ( ( [], True ), 0 ), moveableCards ), table )
        |> (\( ( ( _, _ ), _ ), t ) -> t)


moveFreecellCardWithCheck : Int -> Int -> DeckTable -> DeckTable
moveFreecellCardWithCheck fromCol toCol table =
    let
        maybeFromCard =
            getCardFromTable fromCol table
                |> Tuple.first

        newTable =
            case maybeFromCard of
                Just card ->
                    if checkCardMoveability card toCol table then
                        moveCard fromCol toCol table

                    else
                        table

                Nothing ->
                    table
    in
    newTable


checkCardMoveability : Int -> Int -> DeckTable -> Bool
checkCardMoveability fromCard toCol table =
    let
        ( maybeCard, _ ) =
            getCardFromTable toCol table

        satisfyingColorCondition =
            case maybeCard of
                Just toCard ->
                    xor (numberToSuitColorIsRed fromCard) (numberToSuitColorIsRed toCard)

                Nothing ->
                    False

        satisfyingNumberCondition =
            case maybeCard of
                Just toCard ->
                    (fromCard // 4) + 1 == (toCard // 4)

                Nothing ->
                    False

        satisfyingEmptyCondition =
            case maybeCard of
                Nothing ->
                    True

                Just _ ->
                    False
    in
    satisfyingEmptyCondition || (satisfyingColorCondition && satisfyingNumberCondition)


convertAllHeadOfDeckTableToList : DeckTable -> List (Maybe Int)
convertAllHeadOfDeckTableToList table =
    List.concat [ List.map List.head <| convertDeckTableToList table, convertDeckTableFreecellToList table ]


convertDeckTableToList : DeckTable -> List (List Int)
convertDeckTableToList table =
    let
        (DeckTable list1 list2 list3 list4 list5 list6 list7 list8 _ _ _ _) =
            table
    in
    [ list1, list2, list3, list4, list5, list6, list7, list8 ]


convertDeckTableFreecellToList : DeckTable -> List (Maybe Int)
convertDeckTableFreecellToList table =
    let
        (DeckTable _ _ _ _ _ _ _ _ free1 free2 free3 free4) =
            table
    in
    [ free1, free2, free3, free4 ]


initializeDeck : Int -> DeckTable
initializeDeck num =
    generateRandomList 52 num
        |> List.foldl
            (\randomValue ( leftCount, deckTable, initCardTable ) ->
                let
                    cardTableIdx =
                        randomValue |> modBy leftCount

                    ( cardValue, newCardTable ) =
                        runInitCardTable leftCount cardTableIdx initCardTable

                    col =
                        (52 - leftCount) |> modBy 8

                    newDeckTable =
                        addCardIntoTable cardValue col deckTable
                in
                ( leftCount - 1, newDeckTable, newCardTable )
            )
            ( 52, initialDeckTable, initializerCardTable )
        |> (\( _, a, _ ) -> a)


runInitCardTable : Int -> Int -> Array Int -> ( Int, Array Int )
runInitCardTable leftCount idx table =
    let
        value =
            Array.get idx table
                |> Maybe.withDefault -1

        padValue =
            Array.get (leftCount - 1) table
                |> Maybe.withDefault -1

        newTable =
            Array.set idx padValue table
    in
    ( value, newTable )


moveCard : Int -> Int -> DeckTable -> DeckTable
moveCard fromCol toCol table =
    let
        ( maybeCard, intermediateTable ) =
            getCardFromTable fromCol table
    in
    case maybeCard of
        Just card ->
            addCardIntoTable card toCol intermediateTable

        Nothing ->
            table


checkCardExistent : Int -> DeckTable -> Bool
checkCardExistent col table =
    let
        ( maybeCard, _ ) =
            getCardFromTable col table
    in
    case maybeCard of
        Just _ ->
            True

        Nothing ->
            False


appendCardIntoTable : List Int -> Int -> DeckTable -> DeckTable
appendCardIntoTable cards col table =
    let
        (DeckTable list1 list2 list3 list4 list5 list6 list7 list8 free1 free2 free3 free4) =
            table
    in
    case col of
        0 ->
            DeckTable (List.append (List.reverse cards) list1) list2 list3 list4 list5 list6 list7 list8 free1 free2 free3 free4

        1 ->
            DeckTable list1 (List.append (List.reverse cards) list2) list3 list4 list5 list6 list7 list8 free1 free2 free3 free4

        2 ->
            DeckTable list1 list2 (List.append (List.reverse cards) list3) list4 list5 list6 list7 list8 free1 free2 free3 free4

        3 ->
            DeckTable list1 list2 list3 (List.append (List.reverse cards) list4) list5 list6 list7 list8 free1 free2 free3 free4

        4 ->
            DeckTable list1 list2 list3 list4 (List.append (List.reverse cards) list5) list6 list7 list8 free1 free2 free3 free4

        5 ->
            DeckTable list1 list2 list3 list4 list5 (List.append (List.reverse cards) list6) list7 list8 free1 free2 free3 free4

        6 ->
            DeckTable list1 list2 list3 list4 list5 list6 (List.append (List.reverse cards) list7) list8 free1 free2 free3 free4

        _ ->
            DeckTable list1 list2 list3 list4 list5 list6 list7 (List.append (List.reverse cards) list8) free1 free2 free3 free4


addCardIntoTable : Int -> Int -> DeckTable -> DeckTable
addCardIntoTable card col table =
    let
        (DeckTable list1 list2 list3 list4 list5 list6 list7 list8 free1 free2 free3 free4) =
            table
    in
    case col of
        0 ->
            DeckTable (card :: list1) list2 list3 list4 list5 list6 list7 list8 free1 free2 free3 free4

        1 ->
            DeckTable list1 (card :: list2) list3 list4 list5 list6 list7 list8 free1 free2 free3 free4

        2 ->
            DeckTable list1 list2 (card :: list3) list4 list5 list6 list7 list8 free1 free2 free3 free4

        3 ->
            DeckTable list1 list2 list3 (card :: list4) list5 list6 list7 list8 free1 free2 free3 free4

        4 ->
            DeckTable list1 list2 list3 list4 (card :: list5) list6 list7 list8 free1 free2 free3 free4

        5 ->
            DeckTable list1 list2 list3 list4 list5 (card :: list6) list7 list8 free1 free2 free3 free4

        6 ->
            DeckTable list1 list2 list3 list4 list5 list6 (card :: list7) list8 free1 free2 free3 free4

        7 ->
            DeckTable list1 list2 list3 list4 list5 list6 list7 (card :: list8) free1 free2 free3 free4

        8 ->
            DeckTable list1 list2 list3 list4 list5 list6 list7 list8 (Just card) free2 free3 free4

        9 ->
            DeckTable list1 list2 list3 list4 list5 list6 list7 list8 free1 (Just card) free3 free4

        10 ->
            DeckTable list1 list2 list3 list4 list5 list6 list7 list8 free1 free2 (Just card) free4

        _ ->
            DeckTable list1 list2 list3 list4 list5 list6 list7 list8 free1 free2 free3 (Just card)


getCardsListFromTable : Int -> DeckTable -> List Int
getCardsListFromTable col table =
    let
        (DeckTable list1 list2 list3 list4 list5 list6 list7 list8 _ _ _ _) =
            table
    in
    case col of
        0 ->
            list1

        1 ->
            list2

        2 ->
            list3

        3 ->
            list4

        4 ->
            list5

        5 ->
            list6

        6 ->
            list7

        _ ->
            list8


takeCardsFromTable : Int -> Int -> DeckTable -> ( List Int, DeckTable )
takeCardsFromTable col num table =
    List.range 0 num
        |> List.foldl
            (\idx ( list, deckTable ) ->
                let
                    ( maybeCard, newTable ) =
                        getCardFromTable col deckTable
                in
                ( maybeCard :: list, newTable )
            )
            ( [], table )
        |> (\( list, t ) -> ( List.filterMap identity list, t ))


getCardFromTable : Int -> DeckTable -> ( Maybe Int, DeckTable )
getCardFromTable col table =
    let
        (DeckTable list1 list2 list3 list4 list5 list6 list7 list8 free1 free2 free3 free4) =
            table

        helper list =
            ( List.head list, List.tail list |> Maybe.withDefault [] )
    in
    case col of
        0 ->
            let
                ( maybeValue, newList ) =
                    helper list1
            in
            DeckTable newList list2 list3 list4 list5 list6 list7 list8 free1 free2 free3 free4
                |> Tuple.pair maybeValue

        1 ->
            let
                ( maybeValue, newList ) =
                    helper list2
            in
            DeckTable list1 newList list3 list4 list5 list6 list7 list8 free1 free2 free3 free4
                |> Tuple.pair maybeValue

        2 ->
            let
                ( maybeValue, newList ) =
                    helper list3
            in
            DeckTable list1 list2 newList list4 list5 list6 list7 list8 free1 free2 free3 free4
                |> Tuple.pair maybeValue

        3 ->
            let
                ( maybeValue, newList ) =
                    helper list4
            in
            DeckTable list1 list2 list3 newList list5 list6 list7 list8 free1 free2 free3 free4
                |> Tuple.pair maybeValue

        4 ->
            let
                ( maybeValue, newList ) =
                    helper list5
            in
            DeckTable list1 list2 list3 list4 newList list6 list7 list8 free1 free2 free3 free4
                |> Tuple.pair maybeValue

        5 ->
            let
                ( maybeValue, newList ) =
                    helper list6
            in
            DeckTable list1 list2 list3 list4 list5 newList list7 list8 free1 free2 free3 free4
                |> Tuple.pair maybeValue

        6 ->
            let
                ( maybeValue, newList ) =
                    helper list7
            in
            DeckTable list1 list2 list3 list4 list5 list6 newList list8 free1 free2 free3 free4
                |> Tuple.pair maybeValue

        7 ->
            let
                ( maybeValue, newList ) =
                    helper list8
            in
            DeckTable list1 list2 list3 list4 list5 list6 list7 newList free1 free2 free3 free4
                |> Tuple.pair maybeValue

        8 ->
            DeckTable list1 list2 list3 list4 list5 list6 list7 list8 Nothing free2 free3 free4
                |> Tuple.pair free1

        9 ->
            DeckTable list1 list2 list3 list4 list5 list6 list7 list8 free1 Nothing free3 free4
                |> Tuple.pair free2

        10 ->
            DeckTable list1 list2 list3 list4 list5 list6 list7 list8 free1 free2 Nothing free4
                |> Tuple.pair free3

        _ ->
            DeckTable list1 list2 list3 list4 list5 list6 list7 list8 free1 free2 free3 Nothing
                |> Tuple.pair free4


initialDeckTable : DeckTable
initialDeckTable =
    DeckTable [] [] [] [] [] [] [] [] Nothing Nothing Nothing Nothing


initializerCardTable : Array Int
initializerCardTable =
    List.range 0 51
        |> Array.fromList



-- Random Generator


generateRandomList : Int -> Int -> List Int
generateRandomList length init =
    let
        helper _ ( state, list ) =
            let
                ( randValue, newState ) =
                    rand state
            in
            ( newState, randValue :: list )
    in
    List.repeat length ()
        |> List.foldl helper ( init, [] )
        |> Tuple.second
        |> List.reverse


rand : Int -> ( Int, Int )
rand state =
    let
        newState =
            (state * 214013) + 2531011 |> modBy (2 ^ 31)
    in
    ( newState |> Bitwise.shiftRightZfBy 16, newState )



-- Representation


transformCardStrForConsole : List (List String) -> String
transformCardStrForConsole list =
    let
        newList =
            List.take 8 list

        result =
            List.repeat 7 []
    in
    List.foldr
        (\v res ->
            List.map2 (::) (List.append v [ "" ]) res
        )
        result
        newList
        |> List.map (List.map <| String.padLeft 4 ' ')
        |> List.map String.concat
        |> String.join "\n"


convertDeckTableToCardStr : DeckTable -> List (List String)
convertDeckTableToCardStr =
    List.map (List.map numberToCardStr << List.reverse) << convertDeckTableToList


numberToCardStr : Int -> String
numberToCardStr num =
    String.concat [ numberToNumberStr num, " ", numberToSuites num ]


numberToSuitColorIsRed : Int -> Bool
numberToSuitColorIsRed num =
    case modBy 4 num of
        0 ->
            False

        1 ->
            True

        2 ->
            True

        _ ->
            False


numberToSuites : Int -> String
numberToSuites num =
    case modBy 4 num of
        0 ->
            "♣"

        1 ->
            "♦"

        2 ->
            "♥"

        _ ->
            "♠"


numberToNumberStrTable : List ( Int, String )
numberToNumberStrTable =
    [ ( 0, "A" )
    , ( 1, "2" )
    , ( 2, "3" )
    , ( 3, "4" )
    , ( 4, "5" )
    , ( 5, "6" )
    , ( 6, "7" )
    , ( 7, "8" )
    , ( 8, "9" )
    , ( 9, "10" )
    , ( 10, "J" )
    , ( 11, "Q" )
    , ( 12, "K" )
    ]


numberToNumberStr : Int -> String
numberToNumberStr num =
    let
        idx =
            modBy 13 (num // 4)
    in
    List.filter ((==) idx << Tuple.first) numberToNumberStrTable
        |> List.head
        |> Maybe.map Tuple.second
        |> Maybe.withDefault "souldn't occur"
