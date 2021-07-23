module FreeCell exposing (..)

import Array exposing (Array)
import Bitwise
import StateMonad as SM exposing (StateM)
import Utils


type CardFoundation
    = CardFoundation Int Int Int Int


type DeckTable
    = DeckTable (List Int) (List Int) (List Int) (List Int) (List Int) (List Int) (List Int) (List Int) (List Int)


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
    in
    4 - List.length listf


moveCardWithCheck : Int -> Int -> DeckTable -> DeckTable
moveCardWithCheck fromCol toCol table =
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


convertDeckTableToList : DeckTable -> List (List Int)
convertDeckTableToList table =
    let
        (DeckTable list1 list2 list3 list4 list5 list6 list7 list8 _) =
            table
    in
    [ list1, list2, list3, list4, list5, list6, list7, list8 ]


convertDeckTableFreecellToList : DeckTable -> List Int
convertDeckTableFreecellToList table =
    let
        (DeckTable _ _ _ _ _ _ _ _ listf) =
            table
    in
    listf


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


addCardIntoTable : Int -> Int -> DeckTable -> DeckTable
addCardIntoTable card col table =
    let
        (DeckTable list1 list2 list3 list4 list5 list6 list7 list8 listf) =
            table
    in
    case col of
        0 ->
            DeckTable (card :: list1) list2 list3 list4 list5 list6 list7 list8 listf

        1 ->
            DeckTable list1 (card :: list2) list3 list4 list5 list6 list7 list8 listf

        2 ->
            DeckTable list1 list2 (card :: list3) list4 list5 list6 list7 list8 listf

        3 ->
            DeckTable list1 list2 list3 (card :: list4) list5 list6 list7 list8 listf

        4 ->
            DeckTable list1 list2 list3 list4 (card :: list5) list6 list7 list8 listf

        5 ->
            DeckTable list1 list2 list3 list4 list5 (card :: list6) list7 list8 listf

        6 ->
            DeckTable list1 list2 list3 list4 list5 list6 (card :: list7) list8 listf

        7 ->
            DeckTable list1 list2 list3 list4 list5 list6 list7 (card :: list8) listf

        _ ->
            DeckTable list1 list2 list3 list4 list5 list6 list7 list8 (List.append listf [ card ])


getCardFromTable : Int -> DeckTable -> ( Maybe Int, DeckTable )
getCardFromTable col table =
    let
        (DeckTable list1 list2 list3 list4 list5 list6 list7 list8 listf) =
            table
    in
    case col of
        0 ->
            let
                ( maybeValue, newList ) =
                    case list1 of
                        x :: xs ->
                            ( Just x, xs )

                        [] ->
                            ( Nothing, [] )
            in
            DeckTable newList list2 list3 list4 list5 list6 list7 list8 listf
                |> Tuple.pair maybeValue

        1 ->
            let
                ( maybeValue, newList ) =
                    case list2 of
                        x :: xs ->
                            ( Just x, xs )

                        [] ->
                            ( Nothing, [] )
            in
            DeckTable list1 newList list3 list4 list5 list6 list7 list8 listf
                |> Tuple.pair maybeValue

        2 ->
            let
                ( maybeValue, newList ) =
                    case list3 of
                        x :: xs ->
                            ( Just x, xs )

                        [] ->
                            ( Nothing, [] )
            in
            DeckTable list1 list2 newList list4 list5 list6 list7 list8 listf
                |> Tuple.pair maybeValue

        3 ->
            let
                ( maybeValue, newList ) =
                    case list4 of
                        x :: xs ->
                            ( Just x, xs )

                        [] ->
                            ( Nothing, [] )
            in
            DeckTable list1 list2 list3 newList list5 list6 list7 list8 listf
                |> Tuple.pair maybeValue

        4 ->
            let
                ( maybeValue, newList ) =
                    case list5 of
                        x :: xs ->
                            ( Just x, xs )

                        [] ->
                            ( Nothing, [] )
            in
            DeckTable list1 list2 list3 list4 newList list6 list7 list8 listf
                |> Tuple.pair maybeValue

        5 ->
            let
                ( maybeValue, newList ) =
                    case list6 of
                        x :: xs ->
                            ( Just x, xs )

                        [] ->
                            ( Nothing, [] )
            in
            DeckTable list1 list2 list3 list4 list5 newList list7 list8 listf
                |> Tuple.pair maybeValue

        6 ->
            let
                ( maybeValue, newList ) =
                    case list7 of
                        x :: xs ->
                            ( Just x, xs )

                        [] ->
                            ( Nothing, [] )
            in
            DeckTable list1 list2 list3 list4 list5 list6 newList list8 listf
                |> Tuple.pair maybeValue

        7 ->
            let
                ( maybeValue, newList ) =
                    case list8 of
                        x :: xs ->
                            ( Just x, xs )

                        [] ->
                            ( Nothing, [] )
            in
            DeckTable list1 list2 list3 list4 list5 list6 list7 newList listf
                |> Tuple.pair maybeValue

        8 ->
            let
                ( maybeValue, newList ) =
                    case listf of
                        x :: xs ->
                            ( Just x, xs )

                        [] ->
                            ( Nothing, listf )
            in
            DeckTable list1 list2 list3 list4 list5 list6 list7 list8 newList
                |> Tuple.pair maybeValue

        9 ->
            let
                ( maybeValue, newList ) =
                    case listf of
                        x1 :: x :: xs ->
                            ( Just x, x1 :: xs )

                        _ ->
                            ( Nothing, listf )
            in
            DeckTable list1 list2 list3 list4 list5 list6 list7 list8 newList
                |> Tuple.pair maybeValue

        10 ->
            let
                ( maybeValue, newList ) =
                    case listf of
                        x1 :: x2 :: x :: xs ->
                            ( Just x, x1 :: x2 :: xs )

                        _ ->
                            ( Nothing, listf )
            in
            DeckTable list1 list2 list3 list4 list5 list6 list7 list8 newList
                |> Tuple.pair maybeValue

        _ ->
            let
                ( maybeValue, newList ) =
                    case listf of
                        x1 :: x2 :: x3 :: x :: xs ->
                            ( Just x, x1 :: x2 :: x3 :: xs )

                        _ ->
                            ( Nothing, listf )
            in
            DeckTable list1 list2 list3 list4 list5 list6 list7 list8 newList
                |> Tuple.pair maybeValue


generateRandomList : Int -> Int -> List Int
generateRandomList length init =
    let
        helper _ ( randSM, list ) =
            let
                newState =
                    randSM |> SM.bind rand

                randValue =
                    SM.evalState newState init
            in
            ( newState, randValue :: list )
    in
    List.repeat length ()
        |> List.foldl helper ( initialRandState, [] )
        |> Tuple.second
        |> List.reverse


initialRandState : StateM Int Int
initialRandState =
    SM.return 0


rand : a -> StateM Int Int
rand _ =
    SM.get
        |> SM.bind
            (\state ->
                let
                    newState =
                        (state * 214013) + 2531011 |> modBy (2 ^ 31)
                in
                SM.put newState
                    |> SM.bind
                        (always
                            (SM.return <|
                                (newState |> Bitwise.shiftRightZfBy 16)
                            )
                        )
            )



-- alternative implementation


rand2 : a -> StateM Int Int
rand2 _ =
    SM.get
        |> SM.bind
            (\state ->
                let
                    newState =
                        ((state + 10) |> Bitwise.shiftLeftBy 18) - state * 48131 - 90429 |> modBy (2 ^ 31)
                in
                SM.put newState
                    |> SM.bind
                        (always
                            (SM.return <|
                                ((newState |> Bitwise.shiftLeftBy 1) |> Bitwise.shiftRightZfBy 17)
                            )
                        )
            )


initialDeckTable : DeckTable
initialDeckTable =
    DeckTable [] [] [] [] [] [] [] [] []


initializerCardTable : Array Int
initializerCardTable =
    List.repeat 52 0
        |> List.indexedMap (\idx _ -> idx)
        |> Array.fromList


numberToCardStr : Int -> String
numberToCardStr num =
    String.append (numberToNumberStr num) (numberToSuites num)


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
