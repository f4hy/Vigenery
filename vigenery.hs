-- Vigenery. A vigenere cipher program.
-- Author: Brendan Fahy <brendan@f4hy.com>
-- 
-- This program allows encryption, decryption, and cryptanalysis of
-- files using the viginere cipher.
--
-----------------------------------------------------------------------
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
----------------------------------------------------------------------- 




module Main( main ) where

import System
import System.Console.GetOpt
import Data.Maybe( fromMaybe )
import Control.Monad
import Numeric
import Char
import Vigenere

main = do
  args <- getArgs

  let ( actions, nonOpts, unrec, msgs ) = getOpt' RequireOrder options args

  when (0 < length unrec) $ do putStrLn "Unrecognized Options"
                               print unrec
                               usage
                               exitFailure

  when (0 < length msgs) $ do putStrLn $ unlines msgs
                              usage
                              exitFailure


  opts <- foldl (>>=) (return defaultOptions) actions
  let Options { optInput = input,
                optOutput = output,
                optKey = key,
                optMode = mode
                } = opts

  when (mode == Analyize) $ do putStrLn "Analysing"
                               text <- input
                               analyze $ take 10000 text
                               exitWith ExitSuccess

  when (key == "") $ do putStrLn "A key is required to encrypt or decrypt"
                        usage
                        exitFailure


  when (mode == Encrypt) $  do putStr ("Using " ++key ++ " encrypting ")
                               text <- input
                               output $ encrypt text key
  when (mode == Decrypt) $  do putStrLn ("Decrypting file with" ++ key)
                               text <- input
                               output $ decrypt text key

  exitWith ExitSuccess
  
analyze xs = do putStrLn "Best Kasiski guesses"
                print $ take 15 lengths
                putStrLn "Indicies Of Coincidence"
                print $ map (\x -> showFFloat (Just 4) x "") ics
                putStrLn "The keyword!"
                print $ thekey
                where lengths = filter (>1) $ kasiski xs
                      guess = head lengths
                      thekey = findkey guess xs
                      ics = indiciesOfCoincidence guess xs
                      
        
data Mode = Encrypt | Decrypt | Analyize deriving (Eq)

data Options = Options  {
      optInput  :: IO String,
      optOutput :: String -> IO (),
      optKey :: String,
      optMode :: Mode
    }

defaultOptions :: Options
defaultOptions = Options {
                   optInput  =   do putStrLn "Need input file"
                                    exitFailure,
                                    optOutput = putStrLn,
                                    optKey = "",
                                    optMode = Encrypt
                 }

options :: [OptDescr (Options -> IO Options)]
options = [
    Option ['V'] ["version"]  (NoArg showVersion)         "show version number",
    Option ['i'] ["input"]    (ReqArg readInput "FILE")   "plaintext file to read",
    Option ['o'] ["output"]   (ReqArg writeOutput "FILE") "output file to write" ,
    Option ['k'] ["key"]      (ReqArg getKey "KEYWORD")   "output file to write",
    Option ['e'] ["encrypt"]  (NoArg (setMode Encrypt))   "encrypt plain text",
    Option ['d'] ["decrypt"]  (NoArg (setMode Decrypt))   "decrypt cipher text",
    Option ['a'] ["analysis"] (NoArg (setMode Analyize))  "Run cryptanalysis"
  ]

usage = do
  putStrLn "Usage : vigenery [-mode] [-i input] [-k key]"
  putStrLn "Encryption"
  putStrLn " vigenery -e -i inputfile.txt -k key"
  putStrLn " vigenery -e -i inputfile.txt -k key -o output.txt"
  putStrLn "Decryption"
  putStrLn " vigenery -d -i inputfile.txt -k key"
  putStrLn " vigenery -d -i inputfile.txt -k key -o output.txt"
  putStrLn "cryptAnalysis"
  putStrLn " vigenery -a -i inputfile.txt"


showVersion _ = do
  putStrLn "Version 0.8"
  exitWith ExitSuccess

readInput arg opt = return opt { optInput = myread arg }
writeOutput arg opt = return opt { optOutput = writeFile arg }
getKey arg opt = return opt { optKey = show arg}
setMode arg opt = return opt { optMode = arg }

myread arg = do putStrLn $ "file " ++ arg
                readFile arg

