module Trie exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Set exposing (Set)


type alias Index data =
    { trie : Trie
    , dataDict : Dict Int data
    }


type Trie
    = Empty
    | Trie
        { children : Array Trie
        , data : List Int
        }


type alias Entry data =
    { id : Int
    , tags : List String
    , data : data
    }


type alias DataTags data =
    List ( data, List String )


type alias Data =
    { name : String
    }


infos =
    [ ( "Allan", [ "allan" ] )
    , ( "Allan Nozomu Fukasawa", [ "allan" ] )
    , ( "Alberto", [ "alberto" ] )
    , ( "Allesandra", [ "allesandra" ] )
    , ( "Carlos", [ "carlos" ] )
    , ( "Carla", [ "carla" ] )
    , ( "Carlin", [ "carlin" ] )
    , ( "Eleonor", [ "eleonor" ] )
    , ( "Ellen", [ "ellen", "allan" ] )
    , ( "Elen", [ "elen" ] )
    , ( " Eleonora", [ "eleonora" ] )
    , ( "Ele", [ "ele" ] )
    , ( "El", [ "el" ] )
    ]


infosIndex =
    buildIndex infos


emptyArray : Array Trie
emptyArray =
    Array.initialize 26 (\_ -> Empty)


indexOfChar : Char -> Int
indexOfChar c =
    Char.toCode c - Char.toCode 'a'


buildIndex : DataTags data -> Index data
buildIndex datas =
    let
        entries =
            List.indexedMap (\index ( data, tags ) -> Entry index tags data) datas

        trie =
            List.foldl
                (\curr acc ->
                    addIntoTrie curr acc
                )
                Empty
                entries

        dataDict =
            List.map (\info -> ( info.id, info.data )) entries
                |> Dict.fromList
    in
    Index trie dataDict


addIntoTrie : Entry x -> Trie -> Trie
addIntoTrie entry trie =
    List.foldl
        (\curr acc ->
            addIntoTrieAux entry.id curr acc
        )
        trie
        entry.tags


addIntoTrieAux : Int -> String -> Trie -> Trie
addIntoTrieAux id s trie =
    case String.uncons s of
        Nothing ->
            case trie of
                Empty ->
                    Trie { data = [ id ], children = emptyArray }

                Trie currTrie ->
                    Trie { currTrie | data = id :: currTrie.data }

        Just ( c, ss ) ->
            let
                index =
                    indexOfChar c
            in
            case trie of
                Empty ->
                    let
                        newChild =
                            addIntoTrieAux id ss Empty

                        newChildren =
                            Array.set index newChild emptyArray
                    in
                    Trie { data = [], children = newChildren }

                Trie currTrie ->
                    let
                        currChild =
                            Maybe.withDefault Empty <| Array.get index currTrie.children

                        newChild =
                            addIntoTrieAux id ss currChild

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
