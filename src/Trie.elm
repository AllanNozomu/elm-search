module Trie exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Set exposing (Set)

type alias Index data = 
    {
        trie : Trie,
        dataDict : Dict Int data
    }

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


infosIndex = 
    buildIndex infos


emptyArray : Array Trie
emptyArray =
    Array.initialize 26 (\_ -> Empty)


indexOfChar : Char -> Int
indexOfChar c =
    Char.toCode c - Char.toCode 'a'


buildIndex : List (Entry data) -> Index data
buildIndex entries =
    let
        trie = List.foldl
            (\curr acc ->
                addIntoTrie curr acc
            )
            Empty
            entries

        dataDict = List.map (\info -> ( info.id, info.data )) entries
            |> Dict.fromList 
    in
    Index trie dataDict


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


fetchFromIndex : String -> Index data -> List data
fetchFromIndex s index =
    fetchFromTrie (String.toLower s |> String.toList) index.trie
        |> Set.foldl
            (\curr acc ->
                case Dict.get curr index.dataDict of
                    Nothing ->
                        acc

                    Just data ->
                        data :: acc
            )
            []


fetchFromTrie : List Char -> Trie -> Set Int
fetchFromTrie s trie =
    case trie of
        Empty ->
            Set.empty

        Trie currTrie ->
            case s of
                [] ->
                    getAllMatches trie

                c :: ss ->
                    let
                        index =
                            indexOfChar c
                    in
                    fetchFromTrie ss (Maybe.withDefault Empty <| Array.get index currTrie.children)


getAllMatches : Trie -> Set Int
getAllMatches trie =
    case trie of
        Empty ->
            Set.empty

        Trie currTrie ->
            let
                results =
                    Array.foldl
                        (\curr acc ->
                            Set.union (getAllMatches curr) acc
                        )
                        (Set.fromList currTrie.data)
                        currTrie.children
            in
            results
