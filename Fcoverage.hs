module Fcoverage where 

import Data.List (intersect, intercalate)
import Data.Char (isDigit)
import System.IO
import Control.Monad (liftM, foldM, join)
import Control.Applicative ((<$>), (<*>))
import Text.Printf (printf)

type Branches = [Int]
type Branch = Int
type Fid = Int
type Line = Int
type File = String
data Function = Function Fid String File Line deriving (Show, Eq)

run = do
    f <- readFile "funid"
    b <- readFile "branches"
    c <- readFile "coverage"
    let fromFunid = sequence . (map readFunction) . lines $ f
    let fromBranch = readBranches b
    let fromCoverage = sequence . (map maybeInt) . lines $ c
    let coverages = join $ calcFuncCovg <$> fromFunid <*> fromBranch <*> fromCoverage 
    case (intercalate "\n") . map toString <$> coverages of
      Just v -> putStrLn v
      otherwise -> print "Error"
 
-- funcount File -> [Function]
-- formatted_str = "<function_name>@<file>:<line>
readFunction :: String -> Maybe Function
readFunction str = 
    case words $ delim2space str of
    (fid:name:file:[line]) -> Just (Function (int fid) name file (int line))
    otherwise -> Nothing
    where delim2space str = [if c == '@' || c == ':' then ' '  else c | c <- str]
    
readBranches ::  String -> Maybe [(Int, [Int])]
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

float :: (Int, Int) -> Float
float (x, y) = fromIntegral x / fromIntegral y

int = read :: String -> Int

toString :: (Function, (Int, Int)) -> [Char]
toString ((Function fid name _ _), covg@(x,y)) 
  | y == 0 = printf "%s: No branches" name
  | otherwise = printf "%s: %.2f%% (%d/%d)" name ((float covg) * 100) x y

calcFuncCovg :: [Function]-> [(Fid, [Branch])] -> [Branch] -> Maybe [(Function, (Int, Int))]
calcFuncCovg funcs allBr covered = 
  (sequence . map (getBr allBr)) funcs >>= return . (map (getCovg covered))
  where getBr :: [(Fid, [Branch])] -> Function -> Maybe (Function, [Branch])
        getBr branches f@(Function fid _ _ _) = lookup fid branches >>= return . (,) f
        getCovg :: [Branch] -> (Function, [Branch]) -> (Function, (Int, Int))
        getCovg covered (func, br) = (func, (length $ br `intersect` covered, length br))
    
maybeInt :: String -> Maybe Int
maybeInt str
    | and [isDigit x | x <- str] = Just (read str :: Int)
    | otherwise = Nothing
    
