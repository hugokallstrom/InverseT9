import qualified Data.List as List
import Data.Char
import Data.Ord
import Messages 
import Data.Maybe
import Dictionary

getWords :: [Char] -> [[Char]]
getWords message = (head words):map tail (tail words)
    where words = List.groupBy (\x y -> y /= ' ') message

processWords :: [[Char]] -> [[Int]]
processWords [] = []
processWords (word:rest) = replace word:processWords rest 

convertDict :: [([Char], Integer)] -> [([Int], Int)]
convertDict [] = []
convertDict (tuple:rest) = (replace $ fst tuple, fromIntegral $ snd tuple):convertDict rest

replace :: [Char] -> [Int]
replace [] = []
replace (letter:rest) = (fromIntegral $ snd $ head $ filter (\(x,y) -> x == letter) letterKeys):replace rest

compute :: [[Char]] -> [[Char]]
compute [] = []
compute (firstMess:rest) = (init $ computeMessage numMess words):compute rest
	where numMess = processWords $ getWords firstMess
	      words = getWords firstMess

computeMessage :: [[Int]] -> [[Char]] -> [Char]
computeMessage [] _ = []
computeMessage (firstNumMess:numMess) (firstMess:mess)
	| filter (\(x,y) -> x == firstMess) dictionary == []    = ['?'] ++ ['0'] ++ (computeMessage numMess mess)
	| otherwise = (compareLength (findKeyPresses firstNumMess firstMess)) ++ ['0'] ++ (computeMessage numMess mess)

getRang :: [Char] -> Int
getRang word = fromIntegral $ snd $ fromJust $ List.find (\(x,y) -> x == word) dictionary

findKeyPresses :: [Int] -> [Char] -> [[Char]] 
findKeyPresses [] _ = []
findKeyPresses numWord charWord = (toString numWord ++ (take (rangCompare + dictCompare) $ repeat '^'))
				  :findKeyPresses (init numWord) charWord
	where rangCompare = length $ filter (\(x,y) -> y > getRang charWord) (searchDict numWord)
      	      dictCompare = fromIntegral $ sum $ lexCompare numWord charWord (filter (\(x,y) -> y == getRang charWord) (searchDict numWord))

lexCompare :: [Int] -> [Char] -> [([Int], Int)] -> [Integer]
lexCompare _ _ [] = []
lexCompare numWord word (first:rest) 
	| (compare word dictWord == LT)    = 0:lexCompare numWord word rest
	| (compare word dictWord == GT)    = 1:lexCompare numWord word rest
	| (compare word dictWord == EQ)    = 0:lexCompare numWord word rest
	where dictWord = fst $ dictionary !! (head $ (List.elemIndices first $ convertDict dictionary))

compareLength :: [[Char]] -> [Char]
compareLength keys 
	| length minKeys == 1           = head minKeys
	| last (head minKeys) == '^'    = head minKeys
	| otherwise = compareLength $ tail minKeys 
	where minKeys = filter (\x -> length x <= length (List.minimumBy (comparing length) keys)) keys

searchDict :: [Int] -> [([Int], Int)]
searchDict numWord = filter (\(x, _) -> numWord `List.isPrefixOf` x) dict
	where dict = convertDict dictionary

toString :: [Int] -> [Char]
toString [] = []
toString (int:rest) = intToDigit int:toString rest
