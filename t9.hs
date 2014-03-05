{- Author: Hugo Källström 
 - Date: 4/5 - 2014
 - 
 - Inverse T9 
 - Takes a message as input and returns the least
 - keypresses possible to write the message on 
 - a phone. 
 -}
import qualified Data.List as List
import Data.Char
import Data.Ord
import Messages 
import Data.Maybe
import Dictionary

-- Takes a string as input and returns a list
-- with the words in the string.
getWords :: [Char] -> [[Char]]
getWords message = (head words):map tail (tail words)
    where words = List.groupBy (\x y -> y /= ' ') message

-- Converts a list of words to a list of ints 
-- which is the numeric representation of the words.
processWords :: [[Char]] -> [[Int]]
processWords [] = []
processWords (word:rest) = replace word:processWords rest 

-- Converts all the words in the dictionary 
-- to their respective numeric representation.
convertDict :: [([Char], Integer)] -> [([Int], Int)]
convertDict [] = []
convertDict (tuple:rest) = (replace $ fst tuple, fromIntegral $ snd tuple):convertDict rest

-- Converts a word to its repsective numeric 
-- representation.
replace :: [Char] -> [Int]
replace [] = []
replace (letter:rest) = (fromIntegral $ snd $ head $ filter (\(x,y) -> x == letter) letterKeys):replace rest

-- Takes several messages as input and 
-- returns a list of strings which represent 
-- keypresses on a numeric keyboard.
compute :: [[Char]] -> [[Char]]
compute [] = []
compute (firstMess:rest) = (init $ computeMessage numMess words):compute rest
	where numMess = processWords $ getWords firstMess
	      words = getWords firstMess

-- Takes a list of words in both numeric and alphabetical
-- representation as input. Checks if the word 
-- is in the dictionary, else calls findKeyPresses.
computeMessage :: [[Int]] -> [[Char]] -> [Char]
computeMessage [] _ = []
computeMessage (firstNumMess:numMess) (firstMess:mess)
	| filter (\(x,y) -> x == firstMess) dictionary == []    = ['?'] ++ ['0'] ++ (computeMessage numMess mess)
	| otherwise = (compareLength (findKeyPresses firstNumMess firstMess)) ++ ['0'] ++ (computeMessage numMess mess)

-- Takes a word as input and returns its rang in the dicitionary.
getRang :: [Char] -> Int
getRang word = fromIntegral $ snd $ fromJust $ List.find (\(x,y) -> x == word) dictionary

-- Takes a word in both numerical and alphabetical representation 
-- and calculates the number of modifications to write the word
-- with 1 to n characters. Where n is the length of the word. 
findKeyPresses :: [Int] -> [Char] -> [[Char]] 
findKeyPresses [] _ = []
findKeyPresses numWord charWord = (toString numWord ++ (take (rangCompare + dictCompare) $ repeat '^'))
				  :findKeyPresses (init numWord) charWord
	where rangCompare = length $ filter (\(x,y) -> y > getRang charWord) (searchDict numWord)
      	      dictCompare = fromIntegral $ sum $ lexCompare numWord charWord (filter (\(x,y) -> y == getRang charWord) (searchDict numWord))

-- Calculates the number of words which has a greater 
-- alphabetical ordering.
lexCompare :: [Int] -> [Char] -> [([Int], Int)] -> [Integer]
lexCompare _ _ [] = []
lexCompare numWord word (first:rest) 
	| (compare word dictWord == LT)    = 0:lexCompare numWord word rest
	| (compare word dictWord == GT)    = 1:lexCompare numWord word rest
	| (compare word dictWord == EQ)    = 0:lexCompare numWord word rest
	where dictWord = fst $ dictionary !! (head $ (List.elemIndices first $ convertDict dictionary))

-- Finds the smallest word in the input list.
-- Prioritizes '^' over a letter.
compareLength :: [[Char]] -> [Char]
compareLength keys 
	| length minKeys == 1           = head minKeys
	| last (head minKeys) == '^'    = head minKeys
	| otherwise = compareLength $ tail minKeys 
	where minKeys = filter (\x -> length x <= length (List.minimumBy (comparing length) keys)) keys

-- Searches the dictionary for words starting 
-- with the same numbers as the input word.
searchDict :: [Int] -> [([Int], Int)]
searchDict numWord = filter (\(x, _) -> numWord `List.isPrefixOf` x) dict
	where dict = convertDict dictionary

-- Converts a list of ints to a list of chars.
toString :: [Int] -> [Char]
toString [] = []
toString (int:rest) = intToDigit int:toString rest
