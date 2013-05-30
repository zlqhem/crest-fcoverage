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
    let fromBranch = return $ readBranches b
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
    
readBranches ::  String -> [(Int, [Int])]
readBranches contents =
  split . (map read) . words $ contents 
  where split [] = []
        split (fid:cnt:rest) = (fid, take (cnt * 2) rest) : split (drop (cnt*2) rest) 

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
    
