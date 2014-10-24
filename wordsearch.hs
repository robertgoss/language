module Main where

import Data.Set(fromList,member,Set)
import Data.Char
import Data.List(transpose, subsequences)

import Control.Monad
import Control.Monad.Random(MonadRandom, getRandomR)

--This program expects a wordlist in wordlist.txt
-- To make the word list for this program on standard linux you can use
-- the aspell command as follows: - it is not included as I am not sure about it's license
-- > "aspell dump master > wordlist.txt"


--Some basic type synonms to give greater description.
type Word = String
type WordList = Set Word

type Grid = [[Char]]

--Make a word list from the file wordlist.txt
-- We convert the words to lowercase and remove any with non alphabetic characters
-- so words can be compared without having to check capitialization and as we only want proper
-- words not numbers etc.
makeWordList :: IO WordList
makeWordList = readFile "wordlist.txt" >>= return . fromList . filter isPlainWord . map makeLowerCase . lines
	where makeLowerCase = map toLower 
	      isPlainWord = all isAlpha

--Get the count of the total number of words in a grid using the given wordlist
--Get the lines from the grid corresponding to the 4 directions of words 
-- sum the words found in each of these lines
wordCount :: WordList -> Grid -> Integer
wordCount wordlist grid = sum $ map (wordCountLine wordlist) allLines
    where allLines = concat [horizontalForward, horizontalBackward, verticalForward, verticalBackward]
    	  horizontalForward = grid
    	  horizontalBackward = map reverse horizontalForward
    	  verticalForward = transpose grid
    	  verticalBackward = map reverse verticalForward

--Get the count of the total number of words in a line using the given wordlist
--Split the line into all sublists and count the number whch appear in the wordlist
-- We also require a word to have at least 2 letters.
-- Convert it to an integer to guard against overflow when adding later.
wordCountLine :: WordList -> String -> Integer
wordCountLine wordlist line = fromIntegral . length . filter isWord . subsequences $ line
  where isWord string = length string > 1 && member string wordlist

--Construct a n by n grid of lowercase characters in a random monad
-- Use two replicates to get a list of list of characters.
randomGrid :: (MonadRandom m) => Int -> m Grid
randomGrid n = replicateM n randomLine
  where randomLine = replicateM n randomLetter

--Gets a lowercase letter in the random monad. 
-- Which is a letter in the range from 'a' to 'z'
randomLetter :: (MonadRandom m) => m Char
randomLetter = getRandomR ('a','z')


--Get the average word count for n by n grids with the given word lists.
-- Construct a list of 1000 grids and count the number of words to compute the average
-- Result is given in a random monad.
averageWordCount :: (MonadRandom m) => WordList -> Int -> m Double
averageWordCount wordlist n = do randomGrids <- replicateM 1000 (randomGrid n)
                                 let totalCount = sum $ map (wordCount wordlist) randomGrids
                                 return $ fromIntegral totalCount / 1000

--For each number from 2 to 10 print that number and the average number of words found.
main = do wordlist <- makeWordList
          forM_ [2..10] $ \n ->
             do avgCount <- averageWordCount wordlist n
                putStrLn (show n ++ ": " ++ show avgCount)