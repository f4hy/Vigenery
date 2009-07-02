-- Vigenere.hs This module provides all of the functions used to
-- encrypt/decrypt/crypanalyize the Vigenere cipher
--
-- Author: Brendan Fahy <brendan@f4hy.com>
-----------------------------------------------------------------------
-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation, either version 3 of the
-- License, or (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
------------------------------------------------------------------------

module Vigenere  
( encrypt  
, decrypt  
, kasiski
, indiciesOfCoincidence
, strip
, findkey
) where  

import Char
import Data.List
import qualified Data.Map as Map  
import Maybe

encrypt :: String -> String -> String -- Encrypt plaintext
encrypt s key = z26tostring (zipWith modadd (cycle keystream) plaintext)
    where plaintext = stringtoz26 s
          keystream = stringtoz26 key
          modadd x y = (x+y)`mod`26

decrypt :: String -> String -> String -- Decrypt ciphertext
decrypt s key = z26tostring (zipWith modsubtract (cycle keystream) plaintext)
    where plaintext = stringtoz26 s
          keystream = stringtoz26 key
          modsubtract x y = (y-x)`mod`26

strip ::  String -> String -- remove non alpha characters and change to uppercase
strip = map toUpper . filter isAlpha
--strip x = map toUpper (filter isAlpha x)

stringtoz26 :: String -> [Int] --convert characters to number [0-25]
stringtoz26 x = map ((\x -> x-65) . ord) (strip x)

z26tostring :: [Int] -> String  -- convert numbers 0-25 to characters
z26tostring = map (chr . (+65))

-- pair trigrams with their location in the list
trigrampairs :: (Ord a, Ord t, Num t, Enum t) => [a] -> [([a], t)]
trigrampairs x = sort $ zip (map (trigram x) [0..(l-3)]) [1..]
    where l = length x
          trigram x a = take 3 $ drop a x

-- Find the location of pairs which are repeated.
locrepeatedpairs :: (Ord a, Ord b, Num b, Enum b) => [a] -> [[b]]
locrepeatedpairs x = map snds $ sortBy complengths $filter long $ groupBy fstequ $ trigrampairs x
    where fstequ a b      = fst a == fst b
          complengths a b = compare (length b) (length a)
          long x          = length x > 2
          snds a = [snd b | b<- a]

-- the kasiski test. Find the GCD of the differences in position
kasiski :: (Integral a, Ord a1) => [a1] -> [a]            
kasiski x = map (foldl gcd 0 .diff) (locrepeatedpairs x )
    where diff x = map (subtract $ head x) (tail x)

-- Split a string into groups of n
splitst :: Int -> String -> [String]
splitst _ "" = []
splitst n x  = take n x : splitst n (drop n x)

-- Make n new strings each with every nth letter
nstrings :: Int -> String -> [String]
nstrings n = transpose . splitst n 

-- count the frequencies of each letter
freqcount :: (Num b) => String -> [b]
freqcount xs =  map (\a -> fromIntegral $ length $ elemIndices a xs) allLetters
                   where allLetters = ['A'..'Z']
                         n = length xs

-- calculate the index of coincindence of a list
indexOfCoincidence :: (Fractional b) => String -> b
indexOfCoincidence xs = foldr ((+) . p) 0.0 freq /(n*(n-1.0))
                        where freq = freqcount xs
                              n = fromIntegral $ length xs
                              p f = f*(f-1.0)

-- find the indicies of coincidence of the n substrings
indiciesOfCoincidence :: (Fractional b) => Int -> String -> [b]
indiciesOfCoincidence n = map indexOfCoincidence . nstrings n

-- Get statistical information about a ceaser shift
guessshift :: String -> Int -> Double
guessshift xs g = sum (zipWith (\p f -> (p*f)) probabilities $freqs)/n
               where n     = fromIntegral $ length xs
                     freqs = drop g $ cycle $ freqcount xs
-- Find the key to a ceaser shift
findShift :: String -> Int
findShift xs = fromJust $ elemIndex =<< minimum $ ics
    where ics = map ((\x -> abs $ 0.065-x) . guessshift xs) [0..25] 

-- find the key to a vigenere cipher knowing the key length
findkey :: Int -> String -> String
findkey n = z26tostring . map findShift . nstrings n

probabilities = [0.082,0.015,0.028,0.043,0.127,0.022,0.020,0.061,0.070,0.002,
                 0.008,0.040,0.024,0.067,0.075,0.019,0.001,0.060,0.063,0.091,
                 0.028,0.010,0.023,0.001,0.020,0.001]

-- example = "the almond tree was in tentative blossom. The days were longer, often ending with magnificent evenings of corrugated pink skies. The hunting season was over, with hounds and guns put away for six months. The vineyards were busy again as the well-organized farmers treated their vines and the more lackadaisical neighbors hurried to do the pruning they should have done in November."

-- ciexample = encrypt example "janet"

