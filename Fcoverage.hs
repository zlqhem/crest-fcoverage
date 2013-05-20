module Fcoverage(run) where 

import Data.List (intersect, intercalate)
import System.IO
import Control.Monad (liftM, foldM, join)
import Control.Applicative ((<$>))
import Text.Printf as TP

type Branches = [Int]
type Fid = Int
type Line = Int
type File = String
data Function = Function Fid String File Line deriving (Show, Eq)

-- funid File -> [Function]
-- formatted_str = "<function_name>@<file>:<line>
mkFunction :: String -> Maybe Function
mkFunction str = 
  case words $ delim2space str of
  (fid:name:file:[line]) -> Just (Function (int fid) name file (int line))
  otherwise -> Nothing
  where int = read :: String -> Int
        delim2space str = [if c == '@' || c == ':' then ' '  else c | c <- str]

parseBranches ::  String -> Maybe [(Int, [Int])]
parseBranches contents = 
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
  where int = read ::String -> Int
        nestedIntList = map ((map int) . words) . lines

--coverage :: Branches -> Branches -> (Int, Int)
coverage br covered = (length (br `intersect` covered), length br)

toFloat :: (Int, Int) -> Float
toFloat (x, y) = fromIntegral x / fromIntegral y

--fid2name ::  [Function] -> (Int, Float) -> Maybe (String, Float)
fid2name [] _ = Nothing
fid2name (Function fid name _ _:xs) (id, covg) = if fid == id 
                                 then Just (name, covg) 
                                 else fid2name xs (id, covg) 
--strip :: Maybe [Maybe (String, Float)] -> [(String, Float)]
strip r = case r of
  Just x -> map (\(Just v) -> v) x
  Nothing -> []

formatCovg :: PrintfType r => (String, (Int, Int)) -> r
formatCovg (name, covg@(x,y)) 
  | isNaN (toFloat covg) = TP.printf "%s: No branches" name
  | otherwise = TP.printf "%s: %.2f%% (%d/%d)" name ((toFloat covg) * 100) x y

run = do
  c <- readFile "./coverage"
  let covered = map (read::String -> Int) $ lines c
  r <- readFile "./funid" 
  let fs = map mkFunction (lines r) :: [Maybe Function]
  let functions = map (\(Just f) -> f) $ filter (/= Nothing) fs
  b <- readFile "./branches"
  let Just allFuncBranches = parseBranches b
  let fcovg = map (\(fid, br) -> (fid, coverage br covered)) <$> parseBranches b 
  let result = liftM (map (fid2name functions)) fcovg
  return (strip result) >>= putStrLn . 
                          (intercalate "\n") . (map formatCovg)

