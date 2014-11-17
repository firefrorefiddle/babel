module ParseMoonData where

import Control.Applicative
import Control.Arrow
import Data.Char
import Data.Maybe
import Data.Function
import Data.List
import Debug.Trace
import Text.Printf
import System.FilePath
import qualified Data.Time.Calendar as Gregorian
import qualified Data.Map as M
import Dates

parseFile fp = readFile fp >>= return . parseLines . lines

isStartYearLine = isDigit . head . dropWhile (== '-') . dropWhile isSpace

months = [ ("Jan", 01)
         , ("Feb", 02)
         , ("Mar", 03)
         , ("Apr", 04)
         , ("May", 05)
         , ("Jun", 06)
         , ("Jul", 07)
         , ("Aug", 08)
         , ("Sep", 09)
         , ("Oct", 10)
         , ("Nov", 11)
         , ("Dec", 12)]

type Year = Int
type Month = Int
type Day = Int
type Hour = Int
type Minute = Int

type DateTime = (Year, Month, Day, Hour, Minute)
type Date = (Year, Month, Day)

parseDate :: String -> (Month, Day, Hour, Minute)
parseDate [m1,m2,m3,' ',d1,d2,' ',' ',h1,h2,':',mm1,mm2] =
  ((fromJust $ lookup [m1,m2,m3] months),
   read [d1,d2],
   read [h1,h2],
   read [mm1,mm2])
parseDate d = error $ "malformed date: " ++ d

convert :: (Year, [String]) -> [DateTime]
convert (y, ds) = map (\dstr -> let (m,d,h,mm) = parseDate dstr
                                in (y,m,d,h,mm)) ds

parseLines :: [String] -> [DateTime]
parseLines ls = concatMap convert . go (error "parseLines") [] $ ls
  where go y acc (l:ls)
          | isStartYearLine l = let rest = go
                                           (read .
                                            takeWhile (\c -> isDigit c || c == '-') .
                                            dropWhile isSpace $ l)
                                           []
                                           ((replicate 7 ' ' ++ drop 7 l):ls)
                                in if acc == []
                                   then rest
                                   else ((y, acc):rest)
          | otherwise = let d = take 13 .  drop 8 $ l
                            acc' = if all isSpace d then acc
                                   else acc ++ [d]
                        in go y acc' ls
        go y []  [] = []
        go y acc [] = [(y,acc)]

visibleCrescentThreshold = (8,0)

dir="moon_phases"

allFiles :: [FilePath]
allFiles = map (\x -> if x < 0
                      then printf (dir</>"phases%05d.txt") x
                      else printf (dir</>"phases%04d.txt") x) ([-1999,-1899..3901] :: [Int])

allData = mapM parseFile allFiles >>= return . concat

monthStart :: DateTime -> Gregorian.Day
monthStart (y,m,d,hh,mm) =
  let day = fromCalendar (fromIntegral y) m d
  in if (hh,mm) > visibleCrescentThreshold
     then Gregorian.addDays 2 day
     else Gregorian.addDays 1 day

allMonthDates = do
  parsed <- allData
  let starts = map monthStart parsed
  let ends = map (Gregorian.addDays (-1) . monthStart) $ tail parsed
  let startEnds = zip starts ends
  let grouped = groupBy ((==) `on` (calYear.fst)) $ startEnds
  return . M.fromList . map (\(m:ms) -> ((calYear.fst) m, (m:ms))) $ grouped
