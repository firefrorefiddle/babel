module Dates where

import Data.Time.Calendar
import Data.Time.Calendar.Julian
import Text.Printf

fromCalendar y m d = if (y,m,d) >= (1582,10,15)
                     then fromGregorian y m d
                     else fromJulian y m d
toCalendar dt = if dt >= fromGregorian 1582 10 15
                then toGregorian dt
                else toJulian dt

-- this doesn't handle julian/gregorian conversion correctly
addMonthsC = addGregorianMonthsClip

-- this handles julian/gregorian conversion
addYearsC :: Integer -> Day -> Day
addYearsC n dt = let (y,m,d) = toCalendar dt
                 in if (m,d) == (2,29)
                    then fromCalendar (y+n) m 28
                    else fromCalendar (y+n) m d

offsetCaptivity = 70
offsetFuture = 2520

data HDate -- historic date; some we only know by year,
           -- others more specific
  = HYear  Day -- day and month is 1
  | HMonth Day -- day is 1
  | HDay   Day -- everything specified
  | HTimeSpan HDate HDate
    deriving (Eq)

instance Show HDate where
  show (HYear d) = showYear d
  show (HMonth d) = showMonth d ++ " " ++ showYear d
  show (HDay d) = showMonth d ++ " " ++ showDay d ++ ", " ++ showYear d
  show ts@(HTimeSpan _ _) = 
       let (HTimeSpan d1 d2) = flattenTimespan ts
       in show d1 ++ "-\n" ++ show d2

instance Ord HDate where
  (HYear d1) <= (HYear d2) = d1 <= d2
  (HYear d1) <= (HMonth d2) = d1 <= d2
  (HYear d1) <= (HDay d2) = d1 <= d2
  y@(HYear _) <= (HTimeSpan d2 _) = y <= d2
  (HMonth d1) <= (HYear d2) = d1 <= d2
  (HMonth d1) <= (HMonth d2) = d1 <= d2
  (HMonth d1) <= (HDay d2) = d1 <= d2
  m@(HMonth _) <= (HTimeSpan d2 _) = m <= d2
  (HDay d1) <= (HYear d2) = d1 <= d2
  (HDay d1) <= (HMonth d2) = d1 <= d2
  (HDay d1) <= (HDay d2) = d1 <= d2
  d@(HDay _) <= (HTimeSpan d2 _) = d <= d2
  (HTimeSpan d1 d2) <= y@(HYear _) = d1 <= y
  (HTimeSpan d1 d2) <= m@(HMonth _) = d1 <= m
  (HTimeSpan d1 d2) <= d@(HDay _) = d1 <= d
  (HTimeSpan d1 d2) <= (HTimeSpan d3 d4) = if d1 == d3
                                           then d2 <= d4
                                           else d1 <= d3


equals (HDay d1) (HDay d2) = d1 == d2
equals (HMonth m1) (HDay m2) = m1 == m2
equals (HYear y1) (HYear y2) = y1 == y2
equals (HTimeSpan d1 d2) (HTimeSpan d3 d4) = equals d1 d3 && equals d2 d4
equals _ _ = False

extract f (HYear d) = f d
extract f (HMonth d) = f d
extract f (HDay d) = f d
extract f _ = error "extract applied to timespan"
calYear  dt = let (y, _, _) = toCalendar dt in y
calMonth dt = let (_, m, _) = toCalendar dt in m
calDay   dt = let (_, _, d) = toCalendar dt in d

d1 (HYear y) = y
d1 (HMonth m) = m
d1 (HDay d) = d
d1 (HTimeSpan dt1 _) = d1 dt1

d2 (HYear dt) = fromCalendar (calYear dt) 12 31
d2 (HMonth dt) = addDays (-1) $ addMonthsC 1 dt
d2 (HDay d) = d
d2 (HTimeSpan _ dt2) = d2 dt2

encloses dt1 dt2 = d1 dt1 <= d1 dt2 && d2 dt1 >= d2 dt2 && not (equals dt1 dt2)

showYear d = let (y,_,_) = toCalendar d
             in if y < 1
                then printf "%d B.C.E." (abs $ y-1)
                else printf "%d C.E." y
showMonth d = let (_,m,_) = toCalendar d
              in case m of
                1 -> "Jan."
                2 -> "Feb."
                3 -> "Mar."
                4 -> "Apr."
                5 -> "May"
                6 -> "June"
                7 -> "July"
                8 -> "Aug."
                9 -> "Sept."
                10 -> "Oct."
                11 -> "Nov."
                12 -> "Dec."

showDay d = let (_,_,day) = toCalendar d
              in printf "%2d" day

addDaysH n (HYear y) =
  addDaysH n (HTimeSpan (HDay y)
            (HDay . addDays (-1) . addMonthsC 12 $ y))
addDaysH n (HMonth m) =
  addDaysH n (HTimeSpan (HDay m)
            (HDay . addMonthsC 1 $ m))
addDaysH n (HDay d) = HDay . addDays n $ d
addDaysH n (HTimeSpan ds de) = HTimeSpan
                               (addDaysH n ds)
                               (addDaysH n de)

hYear y = HYear . offset $ fromCalendar y 1 1
hMonth y m = HMonth . offset $ fromCalendar y m 1
hDay y m d = HDay . offset $ fromCalendar y m d
hTimeSpan = HTimeSpan
              
offset hd = let (y, m, d) = toCalendar hd
            in if y == 0
               then error $ "please use the proleptic gregorian calendar without a year 0"
               else if y < 1
                    then fromCalendar (y+1) m d
                    else hd

after, afterSolar :: (String, HDate) -> String -> Integer -> (String, HDate)
after (t, d) t' addD = (t', addDaysH addD d)
afterSolar (t, (HYear y))  t' addY = (t', HYear $  addYearsC addY y)
afterSolar (t, (HMonth y)) t' addY = (t', HMonth $ addYearsC addY y)
afterSolar (t, (HDay y))   t' addY = (t', HDay $   addYearsC addY y)
afterSolar (t, (HTimeSpan d1 d2)) t' addY =
  let (_, d1') = afterSolar (t, d1) t' addY
      (_, d2') = afterSolar (t, d2) t' addY
  in (t', HTimeSpan d1' d2')

extend :: (String, HDate) -> String -> Integer -> (String, HDate)
extend (t, d) t' addD = let (_, d') = after (t,d) undefined addD
                        in (t', HTimeSpan d d')

flattenTimespan (HTimeSpan dt1 dt2) = 
  let dt1' = case dt1 of
               ts@(HTimeSpan _ _) -> HDay $ d1 ts
               other -> other
      dt2' = case dt2 of
               ts@(HTimeSpan _ _) -> HDay $ d2 ts
               other -> other
  in HTimeSpan dt1' dt2'
