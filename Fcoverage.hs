module Fcoverage where 

import Data.List (intersect, intercalate)
import Data.Char (isDigit)
import System.IO
import Control.Monad (liftM, liftM3, foldM, join)
import Control.Applicative ((<$>), (<*>))
import Text.Printf (printf)

type Branch = Int
type Fid = Int
type Line = Int
type File = String
data Function = Function { fid :: Fid, name :: String, path :: File, line :: Line } deriving (Show, Eq)

run = do
    funid <- readFile "funid"
    branches <- readFile "branches"
    coverage <- readFile "coverage"
    let result = calc funid branches coverage
    case result of
      Just v    -> putStrLn v
      Nothing   -> putStrLn "Error"

calc :: String -> String -> String -> Maybe String
calc f b c =  do 
    fromFunid <- sequence . (map readFunction) . lines $ f
    fromBranch <- readBranches b
    fromCoverage <- sequence . (map maybeInt) . lines $ c
    return $ unlines . map toString $ calcFuncCovg fromFunid fromBranch fromCoverage 

readFunction :: String -> Maybe Function
readFunction str = 
    case words $ delim2space str of
    (fid:name:file:[line]) -> Just (Function (int fid) name file (int line))
    otherwise -> Nothing
    where delim2space str = [if c == '@' || c == ':' then ' '  else c | c <- str]
    
readBranches ::  String -> Maybe [(Fid, [Branch])]
readBranches contents = 
  let r = foldM (\acc (x:y:xs) -> if xs /= [] then Nothing else
        case acc of        
          []                -> Just ([(y, x, [])])
          (0, _, _):xs      -> Just ((y, x, []):acc)
          (cnt, fid, br):xs -> Just ((cnt - 1, fid, x:y:br):xs))
        []
        (nestedIntList $ contents) in
  case r of
    Just ((0, _, _):xs) -> fmap (map (\(cnt, fid, br) -> (fid, br))) r
    otherwise -> Nothing
    where nestedIntList = map ((map int) . words) . lines

int = read :: String -> Int

toString :: (String, Int, Int) -> String
toString (name, numBranches, numCoveredBranches) 
  | numBranches == 0 = printf "%s: No branches" name
  | otherwise        = printf "%s: %.2f%% (%d/%d)" name percentage numCoveredBranches numBranches
  where percentage = (fromIntegral numCoveredBranches) / (fromIntegral numBranches) * 100.0 :: Float

calcFuncCovg :: [Function]-> [(Fid, [Branch])] -> [Branch] -> [(String, Int, Int)]
calcFuncCovg funcs allBr covered = 
  [(name f, length branches, length coveredBranches) | f <- funcs,
                                                       let branches = lookup' (fid f) allBr,
                                                       let coveredBranches = branches `intersect` covered]

-- unlike standard lookup, this results in [] instead of nothing when key is not found in assoc
lookup' a [] = []
lookup' a (pair:rest) = if a == fst pair then snd pair else lookup' a rest

maybeInt :: String -> Maybe Int
maybeInt str
    | and [isDigit x | x <- str] = Just (read str :: Int)
    | otherwise = Nothing
    
