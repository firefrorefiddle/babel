module Main where

import Text.Printf
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Dot
import Data.Function
import Data.List
import Debug.Trace
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import HebrewCal
import System.Environment (getArgs)

import Events
import Dates

mkDateGraph :: [(String, HDate)] -> [(String, HDate)] -> [(String, HDate)] ->
               Gr (String, HDate) String
mkDateGraph datesBabel dates360 datesSolar =
  let len = length dates1
      sortDs  = sortBy (compare `on` snd)
      sortDs' = sortBy (compare `on` (snd.snd))
      dates1  = sortDs datesBabel
      dates2  = map (\d -> after360 d "" 2519) dates1
      dates3  = map (\d -> afterSolar d "" 2519) dates1
      nodes1  = zip [1..] dates1
      nodes2  = zip [((+1).fst.last $ nodes1)..] (dates2 ++ dates360)
      nodes3  = zip [((+1).fst.last $ nodes2)..] (dates3 ++ datesSolar)
      edges1  = zipWith (\(x,_) (y,_) -> (x,y,"")) nodes1 (tail nodes1)
      edges2  = zipWith (\(x,_) (y,_) -> (x,y,"")) (sortDs' nodes2) (tail $ sortDs' nodes2)
      edges3  = zipWith (\(x,_) (y,_) -> (x,y,"")) (sortDs' nodes3) (tail $ sortDs' nodes3)
      crossEdges1 = zipWith (\(x,_) (y,_) -> (x, y, "2520*360d (incl.)")) nodes1 nodes2
      crossEdges2 = zipWith (\(x,_) (y,_) -> (x, y, "2520y (incl.)")) nodes1 nodes3
      allNodes   = nodes1 ++ nodes2 ++ nodes3
      allEdges = concat [edges1,edges2,edges3,crossEdges1,crossEdges2]
  in mkGraph allNodes allEdges

markNodesEqual (u, v) g =
  insEdges [(u,v,"="),(v,u,"=")] .
  delEdge (u,v) .
  delEdge (v,u) $ g

unifyEqualNodes (u, v) g =
  let (uctxt, g') = match u g
      (vctxt, g'') = match v g'
  in case (uctxt, vctxt) of
    (Just (uTo, _, (uLabStr, udat), uFrom),
     Just (vTo, _, (vLabStr, _), vFrom)) ->
      let to = nub $ uTo ++ vTo
          from = filter (not . flip elem [u,v] . snd) . nub $ uFrom ++ vFrom
          labStr = intercalate " ;; " . filter (not.null) $ [uLabStr, vLabStr]
          ctxt = (to, u, (labStr, udat), from)
      in ctxt & g''
    _ -> g --  ignore this - it may be that one or both of the nodes
           --  are already merged away
           --  error "unifyEqualNodes couldn't find both distinct nodes"

unifyEnclosingNodes (u, v) g =
  let (uctxt, g') = match u g
      (vctxt, g'') = match v g'
  in case (uctxt, vctxt) of
    (Just (uTo, _, uLab, uFrom),
     Just vctxt'@(vTo, _, vLab, vFrom)) ->
      let uTo' = filter (not . flip elem [v] . snd) . nub $ uTo ++ vTo
          uFrom' = (++ [("encloses", v)]) .
                   nub .
                   filter (not . flip elem [v] . snd) $
                   uFrom ++ vFrom
          vTo' = filter (not . null . fst) vTo
          vFrom' = filter (not . null . fst) vFrom
      in (uTo', u, uLab, uFrom') & ((vTo', v, vLab, vFrom') & g'')          
    _ -> error "unifyEnclosingNodes couldn't find both distinct nodes"

labelDiff (u, v) g =
  let (uctxt, g') = match u g
      (vctxt, g'') = match v g'
  in case (uctxt, vctxt) of
       (Just        (uTo, _, ulab@(_, uLabDat), uFrom),
        Just vctxt'@(_,   _, (_, vLabDat), _)) ->
         let da = d1 vLabDat `diffDays` d1 uLabDat
             db = d2 vLabDat `diffDays` d2 uLabDat
             dc = d1 vLabDat `diffDays` d2 uLabDat
             dd = d2 vLabDat `diffDays` d1 uLabDat
             lab = printf "%d" da -- /%d/%d/%dd" da db dc dd
             uFrom' = ((lab, v):) . filter (not . null . fst) $ uFrom
         in (uTo, u, ulab, uFrom') & g'
       _ -> error "labelDiff couldn't find both distinct nodes"

         
unifyGraphNodes g =
  let eqNodes = [(u, v)
                | (u, ulab) <- labNodes g,
                  (v, vlab) <- labNodes g,
                  u < v, snd ulab `equals` snd vlab]
      g' = foldr unifyEqualNodes g eqNodes

      {- quite unreadable
      encNodes = [(u, v)
                | (u, ulab) <- labNodes g',
                  (v, vlab) <- labNodes g',
                  u /= v,
                  snd ulab `encloses` snd vlab]
      g'' = foldr unifyEnclosingNodes g' encNodes
      -}

      unlabeled = [(u, v) | (u, v, l) <- labEdges g', l == ""]
      g'' = foldr labelDiff g' unlabeled

  in g''

readDates fp = do
  evs <- readEvents fp
  case (lookup "datesBabel" evs,
        lookup "dates360" evs,
        lookup "datesSolar" evs) of
    (Just babel, Just d360, Just solar) -> return (babel, d360, solar)
    _ -> error "Could not find all required groups in events file. Need datesBabel, dates360 and datesSolar."

mkGraphic fp = do (datesBabel, dates360, datesSolar) <- readDates fp
                  let gr = unifyGraphNodes $ mkDateGraph datesBabel dates360 datesSolar
                  return $ fglToDotGeneric gr showNode id id
  where showNode (str, d) = pretty str ++ "\n" ++ show d ++ "\n" ++ showHebrew d ++ showWeekDay d
        threshold = 25
        pretty str | length str < threshold = str
                   | otherwise = let w = words str
                                     (taken, rest) = takeSome 0 w
                                 in case rest of
                                   [] -> unwords taken
                                   rest' -> unwords taken ++ "\n" ++ pretty rest'
        takeSome n [] = ([], [])
        takeSome n (w:ws) | n > threshold = ([], unwords (w:ws))
                          | otherwise = let (taken', rest) = takeSome (n+1+length w) ws
                                        in (w:taken', rest)
        showWeekDay (HDay dt) = let (_,_,wd) = toWeekDate dt
                                in "\n"++showWeekDay' wd
        showWeekDay _ = ""
        showWeekDay' 1 = "Monday"
        showWeekDay' 2 = "Tuesday"
        showWeekDay' 3 = "Wednesday"
        showWeekDay' 4 = "Thursday"
        showWeekDay' 5 = "Friday"
        showWeekDay' 6 = "Sabbath"
        showWeekDay' 7 = "Sunday"


writeIt = mkGraphic "Events.txt" >>= writeFile "babel.dot" . showDot 

main = do
  args <- getArgs
  evs <- case args of
      [] -> mkGraphic "Events.txt"
      (fp:_) -> mkGraphic fp
  writeFile "babel.dot" $ showDot evs
