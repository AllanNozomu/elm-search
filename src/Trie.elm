module Trie exposing (..)

import Array exposing (Array)
import Dict
import Set exposing (Set)


type Trie
    = Empty
    | Trie
        { children : Array Trie
        , data : List Int
        }


type alias Entry data =
    { id : Int
    , tags : String
    , data : data
    }


type alias Data =
    { name : String
    }


infos =
    [ Entry 1 "allan" <| Data "Allan"
    , Entry 2 "allan" <| Data "Allan Nozomu Fukasawa"
    , Entry 3 "alberto" <| Data "Alberto"
    , Entry 4 "allesandra" <| Data "Allesandra"
    , Entry 5 "carlos" <| Data "Carlos"
    , Entry 6 "carla" <| Data "Carla"
    , Entry 7 "carlin" <| Data "Carlin"
    , Entry 8 "eleonor" <| Data "Eleonor"
    , Entry 9 "ellen" <| Data "Ellen"
    , Entry 10 "elen" <| Data "Elen"
    , Entry 11 "eleonora" <| Data " Eleonora"
    , Entry 12 "ele" <| Data "Ele"
    , Entry 13 "el" <| Data "El"
    ]


infosTrie =
    List.foldl
        (\curr acc ->
            addIntoTrie curr acc
        )
        Empty
        infos


infosDict =
    List.map (\info -> ( info.id, info )) infos
        |> Dict.fromList


emptyArray : Array Trie
emptyArray =
    Array.initialize 26 (\_ -> Empty)


indexOfChar : Char -> Int
indexOfChar c =
    Char.toCode c - Char.toCode 'a'


addIntoTrie : Entry x -> Trie -> Trie
addIntoTrie entry trie =
    addIntoTrieAux entry entry.tags trie


addIntoTrieAux : Entry x -> String -> Trie -> Trie
addIntoTrieAux entry s trie =
    case String.uncons s of
        Nothing ->
            case trie of
                Empty ->
                    Trie { data = [ entry.id ], children = emptyArray }

                Trie currTrie ->
                    Trie { currTrie | data = entry.id :: currTrie.data }

        Just ( c, ss ) ->
            let
                index =
                    indexOfChar c
            in
            case trie of
                Empty ->
                    let
                        newChild =
                            addIntoTrieAux entry ss Empty

                        newChildren =
                            Array.set index newChild emptyArray
                    in
                    Trie { data = [], children = newChildren }

                Trie currTrie ->
                    let
                        currChild =
                            Maybe.withDefault Empty <| Array.get index currTrie.children

                        newChild =
                            addIntoTrieAux entry ss currChild

                        newChildren =
                            Array.set index newChild currTrie.children
                    in
                    Trie { currTrie | children = newChildren }


fetchFromTrie : String -> Trie -> List Data
fetchFromTrie s trie =
    fetchFromTrieAux (String.toLower s |> String.toList) trie
        |> Set.foldl
            (\curr acc ->
                case Dict.get curr infosDict of
                    Nothing ->
                        acc

                    Just data ->
                        data.data :: acc
            )
            []


fetchFromTrieAux : List Char -> Trie -> Set Int
fetchFromTrieAux s trie =
    case trie of
        Empty ->
            Set.empty

        Trie currTrie ->
            case s of
                [] ->
                    fetchAllMatches trie

                c :: ss ->
                    let
                        index =
                            indexOfChar c
                    in
                    fetchFromTrieAux ss (Maybe.withDefault Empty <| Array.get index currTrie.children)


fetchAllMatches : Trie -> Set Int
fetchAllMatches trie =
    case trie of
        Empty ->
            Set.empty

        Trie currTrie ->
            let
                results =
                    Array.foldl
                        (\curr acc ->
                            Set.union (fetchAllMatches curr) acc
                        )
                        (Set.fromList currTrie.data)
                        currTrie.children
            in
            results
