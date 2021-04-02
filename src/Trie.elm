module Trie exposing (..)

import Array exposing (Array)


type Trie d
    = Empty
    | Trie
        { children : Array (Trie d)
        , data : List d
        }


type alias Info =
    { tags : String
    , name : String
    }


infos =
    [ Info "allan" "Allan"
    , Info "allan" "Allan Nozomu Fukasawa"
    , Info "alberto" "Alberto"
    , Info "allesandra" "Allesandra"
    , Info "carlos" "Carlos"
    , Info "carla" "Carla"
    , Info "carlin" "Carlin"
    , Info "eleonor" "Eleonor"
    , Info "ellen" "Ellen"
    , Info "elen" "Elen"
    , Info "eleonora" "Eleonora"
    , Info "ele" "Ele"
    , Info "el" "El"
    ]


trieInfos =
    List.foldl
        (\curr acc ->
            addIntoTrie curr acc
        )
        Empty
        infos


emptyArray : Array (Trie d)
emptyArray =
    Array.initialize 26 (\_ -> Empty)


indexOfChar : Char -> Int
indexOfChar c =
    Char.toCode c - Char.toCode 'a'


addIntoTrie : Info -> Trie Info -> Trie Info
addIntoTrie info trie =
    addIntoTrieAux info (String.toList info.tags) trie


addIntoTrieAux : Info -> List Char -> Trie Info -> Trie Info
addIntoTrieAux info s trie =
    case s of
        [] ->
            case trie of
                Empty ->
                    Trie { data = [ info ], children = emptyArray }

                Trie currTrie ->
                    Trie { currTrie | data = info :: currTrie.data }

        c :: ss ->
            let
                index =
                    indexOfChar c
            in
            case trie of
                Empty ->
                    let
                        newChild =
                            addIntoTrieAux info ss Empty

                        newChildren =
                            Array.set index newChild emptyArray
                    in
                    Trie { data = [], children = newChildren }

                Trie currTrie ->
                    let
                        currChild =
                            Maybe.withDefault Empty <| Array.get index currTrie.children

                        newChild =
                            addIntoTrieAux info ss currChild

                        newChildren =
                            Array.set index newChild currTrie.children
                    in
                    Trie { currTrie | children = newChildren }


fetchFromTrie : String -> Trie Info -> List Info
fetchFromTrie s trie =
    fetchFromTrieAux (String.toLower s |> String.toList) trie


fetchFromTrieAux : List Char -> Trie Info -> List Info
fetchFromTrieAux s trie =
    case trie of
        Empty ->
            []

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


fetchAllMatches : Trie Info -> List Info
fetchAllMatches trie =
    case trie of
        Empty ->
            []

        Trie currTrie ->
            let
                results =
                    Array.foldl
                        (\curr acc ->
                            fetchAllMatches curr ++ acc
                        )
                        currTrie.data
                        currTrie.children
            in
            results
