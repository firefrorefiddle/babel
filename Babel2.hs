module Main where

import Data.Time.Calendar
import Data.Time.Calendar.Julian
import Text.Printf
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Dot
import Text.Dot
import Data.Function
import Data.List
import Debug.Trace
import System.Environment (getArgs)

import Events
import Dates

data OffsetType = O360 | OS
                deriving (Read, Show, Eq, Ord)
                         
data NodeType = Ancient | Modern | Offset OffsetType
                deriving (Read, Show, Eq, Ord)


tag tp (lab, dt) = (tp, lab, dt)
tfst (x,_,_) = x
thrd (_,_,x) = x

offsetDateNodes :: [(Node, (NodeType, String, HDate))] -> Node -> [(Node, (NodeType, String, HDate), (Node, String))]
offsetDateNodes dates firstNode = concat $ 
    zipWith (\(n, (_, d, l)) m -> 
                 [(m,   tag (Offset O360) (after360 (d,l) "" (offsetFuture)), (n, "2520*360d")),
                  (m+1, tag (Offset OS) (afterSolar (d,l) "" (offsetFuture)), (n, "2520y"))])
            dates
            [firstNode,firstNode+2..]


readDates fp = do
  evs <- readEvents fp
  case (lookup "datesBabel" evs,
        lookup "datesModern" evs) of
    (Just babel, Just modern) -> return (babel,modern)
    _ -> error "Could not find all required groups in Events.txt. Need datesBabel and datesModern."


mkDateGraph :: [(String, HDate)] -> [(String, HDate)] -> Gr (NodeType, String, HDate) String
mkDateGraph datesBabel datesModern =
  let nodes1  = zip [1..] (map (tag Ancient) datesBabel)
      offsets :: [(Node, (NodeType, String, HDate), (Node, String))]      
      offsets = offsetDateNodes nodes1 (length nodes1 + 1)
      nodes2  = map (\(n, l, _) -> (n, l)) offsets      
      nodes3  = zip [length nodes1 + length nodes2 + 1..] (map (tag Modern) datesModern)
      allNodes, nodes1, nodes2, nodes3 :: [(Node, (NodeType, String, HDate))]
      allNodes  = nodes1 ++ nodes2 ++ nodes3
      pairEdges ns = let ns' = sortBy (compare `on` (thrd.snd)) ns
                     in zipWith (\(x,_) (y,_) -> (x,y,"")) ns' (tail ns')
      edges   = concatMap pairEdges .
                groupBy ((==) `on` (tfst.snd)) .
                sortBy (compare `on` (tfst.snd)) $ nodes1 ++ nodes2 -- allNodes
      crossEdges = map (\(n2, _, (n1, l)) -> (n1,n2,l)) offsets      
      allEdges  = concat [edges, crossEdges]
  in mkGraph allNodes allEdges

markNodesEqual (u, v) g =
  insEdges [(u,v,"="),(v,u,"=")] .
  delEdge (u,v) .
  delEdge (v,u) $ g

unifyEqualNodes (u, v) g =
  let (uctxt, g') = match u g
      (vctxt, g'') = match v g'
  in case (uctxt, vctxt) of
    (Just (uTo, _, (uTp, uLabStr, udat), uFrom),
     Just (vTo, _, (vTp, vLabStr, _), vFrom)) ->
      let to = nub $ uTo ++ vTo
          from = filter (not . flip elem [u,v] . snd) . nub $ uFrom ++ vFrom
          labStr = intercalate " ;; " . filter (not.null) . nub $ [uLabStr, vLabStr]
          ctxt = (to, u, (uTp, labStr, udat), from)
      in ctxt & g''
    _ -> g --  ignore this - it may be that one or both of the nodes
           --  are already merged away
           --  error "unifyEqualNodes couldn't find both distinct nodes"

connectOffsetsToModern g =
  let threshold = 360
      connected = [(u, v, printf "start %d days before start of" (d1 dtv `diffDays` d1 dtu))
                  | (u, (Offset _, _, dtu)) <- labNodes g
                  , (v, (Modern, _, dtv)) <- labNodes g
                  , dtv >= dtu
                  , not (dtv `equals` dtu)
                  , not (dtu `encloses` dtv)
                  , d1 dtv `diffDays` d1 dtu < threshold] ++
                  [(u, v, printf "end %d days before end of" (d2 dtv `diffDays` d2 dtu))
                  | (u, (Offset _, _, dtu)) <- labNodes g
                  , (v, (Modern, _, dtv)) <- labNodes g
                  , d2 dtv >= d2 dtu
                  , not (dtv `equals` dtu)
                  , not (dtu `encloses` dtv)                    
                  , d2 dtv `diffDays` d2 dtu < threshold] ++
                  [(v, u, printf "start %d days before start of" (d1 dtu `diffDays` d1 dtv))
                  | (u, (Offset _, _, dtu)) <- labNodes g
                  , (v, (Modern, _, dtv)) <- labNodes g
                  , dtv <= dtu
                  , not (dtv `equals` dtu)
                  , not (dtv `encloses` dtu)
                  , d1 dtu `diffDays` d1 dtv < threshold] ++
                  [(v, u, printf "end %d days before end of" (d2 dtu `diffDays` d2 dtv))
                  | (u, (Offset _, _, dtu)) <- labNodes g
                  , (v, (Modern, _, dtv)) <- labNodes g
                  , d2 dtu >= d2 dtv
                  , not (dtv `equals` dtu)
                  , not (dtv `encloses` dtu)                    
                  , d2 dtu `diffDays` d2 dtv < threshold] ++
                  [(u, v, "contains")
                  | (u, (Offset _, _, dtu)) <- labNodes g
                  , (v, (Modern, _, dtv)) <- labNodes g
                  , dtu `encloses` dtv] ++
                  [(v, u, "contains")
                  | (u, (Offset _, _, dtu)) <- labNodes g
                  , (v, (Modern, _, dtv)) <- labNodes g
                  , dtv `encloses` dtu] ++
                  [(u, v, "same as")
                  | (u, (Offset _, _, dtu)) <- labNodes g
                  , (v, (Modern, _, dtv)) <- labNodes g
                  , dtv `equals` dtu]
  in foldr insEdge g connected
                  
removeUninterestingOffsets g =
  let candidates = [u | (u, (Offset _, _, _)) <- labNodes g]
  in foldr maybeDeleteU g candidates
  where maybeDeleteU u g = case match u g of
                                (Nothing, _) -> g
                                (Just (inc, _, _, outg), g') ->
                                    if length (filter (not.null.fst) inc) < 2 &&
                                       length (filter (not.null.fst) outg) < 1
                                    then case (filter (null.fst) inc, filter (null.fst) outg) of
                                      ([(_,pred)], [(_,succ)]) -> case match pred g' of
                                        (Nothing, _) -> g'
                                        (Just (pinc, _, plab, poutg), g'') ->
                                          (pinc, pred, plab, (("",succ):poutg)) & g''
                                      _ -> g'
                                    else g

removeUninterestingModerns g =
  let candidates = [u | (u, (Modern, _, _)) <- labNodes g]
  in foldr maybeDeleteU g candidates
  where maybeDeleteU u g = case match u g of
                                (Nothing, _) -> g
                                (Just (inc, _, _, outg), g') ->
                                    if length (filter (not.null.fst) inc) < 1 &&
                                       length (filter (not.null.fst) outg) < 1
                                    then case (filter (null.fst) inc, filter (null.fst) outg) of
                                      ([(_,pred)], [(_,succ)]) -> case match pred g' of
                                        (Nothing, _) -> g'
                                        (Just (pinc, _, plab, poutg), g'') ->
                                          (pinc, pred, plab, (("",succ):poutg)) & g''
                                      _ -> g'
                                    else g                                         

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
                  u < v,
                  tfst ulab == tfst vlab,
                  thrd ulab `equals` thrd vlab]
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
--      g'' = foldr labelDiff g' unlabeled

  in g'

mkGraphic fp = do (datesBabel, datesModern) <- readDates fp
                  let gr = unifyGraphNodes .
                           removeUninterestingOffsets .
                           removeUninterestingModerns .                     
                           connectOffsetsToModern $
                           mkDateGraph datesBabel datesModern
                  return $ do fglToDotGeneric gr showNode id id
                              mapM_ allSameRank .
                                filter ((/= Modern) . tfst . snd . head) .
                                groupBy ((==) `on` (tfst . snd)) .
                                sortBy (compare `on` (tfst . snd)) $ labNodes gr
  where showNode (_, str, d) = pretty str ++ "\n" ++ show d
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
        allSameRank ns = same $ map (userNodeId . fst) ns


writeIt = mkGraphic "Events.txt" >>= writeFile "babel2.dot" . showDot 

main = do
  args <- getArgs
  evs <- case args of
      [] -> mkGraphic "Events.txt"
      (fp:_) -> mkGraphic fp
  writeFile "babel2.dot" $ showDot evs
