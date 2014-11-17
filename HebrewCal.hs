module HebrewCal where

import ParseMoonDataBS
import Data.IORef
import System.IO.Unsafe
import qualified Data.Map as M
import Data.Time.Calendar as Gregorian
import Data.List
import Data.Function
import Data.Maybe
import Dates
import Debug.Trace
import Text.Printf
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import qualified Data.ByteString.Lazy as L
import Control.Applicative
import System.Directory

type MonthRange = (Gregorian.Day, Gregorian.Day)

instance Binary Gregorian.Day where
         put (ModifiedJulianDay d) = put d
         get = do i <- get
                  return $ ModifiedJulianDay i

cacheFile = ".monthmap.cache"

writeLookupMapCache m = L.writeFile cacheFile . runPut . put $ m

readLookupMapCache :: IO (M.Map Integer [MonthRange])
readLookupMapCache = do
   bs <- L.readFile cacheFile
   return $ runGet get bs

monthStartMap :: IORef (M.Map Integer [MonthRange])
monthStartMap = unsafePerformIO $ do
              readCache <- doesFileExist cacheFile
              m <- if readCache
                   then readLookupMapCache
                   else do ds <- allMonthDates 
                           writeLookupMapCache ds
                           return ds
              newIORef m

lookupY :: Integer -> Maybe [MonthRange]
lookupY y = unsafePerformIO $ readIORef monthStartMap >>= return . M.lookup y

nisanDate :: Integer -> MonthRange
nisanDate y = 
  case lookupY y of
    Nothing -> error "year out of range"
    Just months -> minimumBy (compare `on`
                              (abs . diffDays (fromCalendar y 3 21) . fst)) months

yearMonths :: HDate -> [MonthRange]
yearMonths y = let y' = extract calYear y
                   nd1 = nisanDate y'
                   nd2 = nisanDate (y'+1)
               in case (lookupY y', lookupY (y'+1)) of
                 (Just m1s, Just m2s) ->
                   dropWhile (< nd1) m1s ++ takeWhile (< nd2) m2s
                 _ -> error "yearMonths"

findMonth :: HDate -> MonthRange
findMonth (HTimeSpan _ _) = error "findMonth applied to timespan"
findMonth d = let months = concatMap (fromJust . lookupY) [extract calYear d - 1..]
              in head . dropWhile ((< extract id d).snd) $ months

findHeMonth :: HDate -> MonthRange
findHeMonth (HTimeSpan _ _) = error "findHeMonth applied to timespan"
findHeMonth dt = let caly = extract calYear dt
                     offset = if caly < 1 then 1 else 0
                     prevYear = hYear $ caly - 1 - offset
                     months = yearMonths prevYear ++ yearMonths dt
                 in head . dropWhile ((< dt) . HDay . snd) $ months
                 
heDay :: Integer -> Int -> Int -> HDate
heDay y m d = let months = yearMonths (hYear y)
              in HDay $ Gregorian.addDays (fromIntegral d-1) (fst $ months !! (m-1))

heMonth :: Integer -> Int -> HDate
heMonth y m = let (start, end) = findHeMonth (heDay y m 1)
              in HTimeSpan (HDay start) (HDay end)

heYear :: Integer -> HDate
heYear y = let (HTimeSpan start _) = heMonth y 1
               (HTimeSpan end   _) = heMonth (y+1) 1
           in HTimeSpan start (HDay . addDays (-1) . extract id $ end)

toHebrew :: Gregorian.Day -> (Integer, Int, Int)
toHebrew dt = let y = calYear dt
                  dd  = fromIntegral . Gregorian.diffDays dt
                  m1s = yearMonths (HYear $ fromCalendar (y-1) 1 1)
                  m2s = yearMonths (HYear $ fromCalendar  y    1 1)
                  containsDt (_, (start, end)) = start <= dt && end >= dt
                  found1 = filter containsDt (zip [1..] m1s)
                  found2 = filter containsDt (zip [1..] m2s)
                  offset = if y < 1 then 1 else 0
              in case (found1, found2) of
                     ([(i,(start,_end))], []) -> (y-1-offset, i, dd start+1)
                     ([], [(i,(start,_end))]) -> (y  -offset, i, dd start+1)
                     _ -> error $ "toHebrew: " ++ show (m1s, m2s, dt, y)

showHebrew :: HDate -> String
showHebrew (HDay dt) = let (y, m, d) = toHebrew dt
                       in printf "%s %d, %s" 
                          (showHeMonth m) 
                          d 
                          (showHeYear y m)
showHebrew (HMonth dt) = let (y, m, d) = toHebrew dt
                             (y2,m2,d2) = toHebrew . Gregorian.addDays (-1) . addMonthsC 1 $ dt
                         in showHebrew (HTimeSpan (heDay y m d) (heDay y2 m2 d2))
showHebrew (HYear dt) =  let (y, m, d) = toHebrew dt
                             (y2,m2,d2) = toHebrew . Gregorian.addDays (-1) . addYearsC 1 $ dt
                         in showHebrew (HTimeSpan (heDay y m d) (heDay y2 m2 d2))
showHebrew ts@(HTimeSpan _ _) = 
           let (HTimeSpan dt1 dt2) = flattenTimespan ts
           in  showHebrew (HDay $ d1 dt1) ++ "-\n" ++ showHebrew (HDay $ d2 dt2)

-- official jewish year
-- yearOffset = 5774 - 2014

-- according to torahcalendar.com
yearOffset = 6000 - 2014

showHeYear y m = let civilCorrection = if m > 6
                                       then 0
                                       else (-1)
                     bcCorrection = if y < 1
                                    then 1
                                    else 0
                 in show $ y + yearOffset + civilCorrection + bcCorrection

showHeMonth :: Int -> String
showHeMonth 1 = "Nissan"
showHeMonth 2 = "Iyar"
showHeMonth 3 = "Sivan"
showHeMonth 4 = "Tammuz"
showHeMonth 5 = "Av"
showHeMonth 6 = "Elul"
showHeMonth 7 = "Tishri"
showHeMonth 8 = "Cheshvan"
showHeMonth 9 = "Kislev"
showHeMonth 10 = "Tevet"
showHeMonth 11 = "Shevat"
showHeMonth 12 = "Adar"
showHeMonth 13 = "Adar II"
