module Lib.GeneticAlg exposing (..)

import Array exposing (..)
import List.Extra as ListEx exposing (..)
import Pixels exposing (..)
import Point2d exposing (..)
import Random exposing (..)
import Random.Array as RandArr exposing (..)
import String.Extra as StrEX exposing (..)
import Vector2d exposing (..)


type alias DNA =
    { genes : String
    , fitness : Int
    }


type Msg
    = CrossOver (Array ( DNA, DNA, Int ))


calcFitness : String -> String -> Int
calcFitness dna allPhrase =
    let
        dnaList =
            String.toList dna

        allPhraseList =
            String.toList allPhrase
    in
    List.length <|
        List.filter (\( a, b ) -> a == b) <|
            ListEx.zip dnaList allPhraseList


makeCrossOver : DNA -> DNA -> Generator (Array Int)
makeCrossOver genes partner =
    RandArr.array (String.length genes.genes) (int 0 100)



-- type alias Model u c =
--     {
--     }


popmax =
    200


mutationRate =
    0.01



-- -- View
-- view model =
