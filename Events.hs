{-# LANGUAGE NoMonomorphismRestriction #-}
module Events (readEvents) where

import Dates
import HebrewCal
import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Combinator
import Text.Printf
import Data.Maybe (fromJust)
import Control.Applicative ((<$>))
import Control.Monad
import Data.Char
import Data.List
import Debug.Trace
import qualified Data.Map.Strict as M


-- |Read and parse events file.
readEvents :: FilePath -> IO [(GroupId, [(String, HDate)])]
readEvents fp = do
  parseRes <- parseFile fp
  case parseRes of
    Left err -> error $ "Parse error in events file " ++ fp ++ "\n" ++
                show err
    Right topExps -> return $ toSimpleEventList topExps


langDef = LanguageDef
   { commentStart = ""
   , commentEnd = ""
   , commentLine = "--"
   , nestedComments = True
   , identStart = letter
   , identLetter = alphaNum <|> char '_' <|> char '-'
   , opStart = fail ""
   , opLetter = fail ""
   , reservedNames = ["after", "extend", "begin", "end", "group"]
   , reservedOpNames = []
   , caseSensitive = False
   }

tokenParser = makeTokenParser langDef
lexeme' = lexeme tokenParser
whiteSpace' = whiteSpace tokenParser
eventId = identifier tokenParser
reserved' = reserved tokenParser

type EventId = String
type GroupId = String

data Event = Event
             { eId   :: Maybe EventId
             , eText :: String
             , eDate :: DateSpec }
           deriving (Read, Show, Eq)

data DateSpec = DateSpecEventId EventId
              | DateSpecAfter TimeSpec DateSpec
              | DateSpecExtend TimeSpec DateSpec
              | DateSpecStartEnd DateSpec DateSpec
              | DateSpec DateType Int (Maybe Int) (Maybe Int)
              deriving (Read, Show, Eq)

data DateType = Regular | Hebrew
              deriving (Read, Show, Eq)

data TimeSpec = TimeSpec TimeType Int
              deriving (Read, Show, Eq)

data TimeType = Days | Years | Years360
              deriving (Read, Show, Eq)

data TopExp = TEEvent Event
            | TEGroup GroupId [EventSpec]
              deriving (Read, Show, Eq)

data EventSpec = ESEvent Event
               | ESId EventId
               | ESGroup String
              deriving (Read, Show, Eq)
                       
fakeNewlineChar = '\0'
fakeNewline = lexeme tokenParser $ char fakeNewlineChar >> return ()
                       
int = fromIntegral <$> integer tokenParser
                       
strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse

eventSep = reservedOp tokenParser "|"
  
eventWord = many1 $ satisfy (not . flip elem ['|', '\n', '\0'])

mEventParticle = lexeme' $ (Just <$> eventId) <|> (lookAhead eventSep >> return Nothing)

eventParticle = lexeme tokenParser eventWord

dateSpec = try dateSpecStartEnd <|> dateSpec'
dateSpec'
  =   try dateSpecId
  <|> try dateSpecAfter
  <|> try dateSpecExtend
  <|> dateSpecDirect

dateSpecId = DateSpecEventId <$> identifier tokenParser

dateSpecAfter = do reserved tokenParser "after" 
                   t <- timeSpec
                   d <- dateSpec
                   return $ DateSpecAfter t d

dateSpecExtend = do reserved tokenParser "extend"
                    t <- timeSpec
                    d <- dateSpec
                    return $ DateSpecExtend t d

dateSpecStartEnd = do d1 <- dateSpec'
                      eventSep
                      d2 <- dateSpec'
                      return $ DateSpecStartEnd d1 d2

dateSpecDirect = do year <- int
                    tp <- option Regular (char 'h' >>
                                          whiteSpace' >>
                                          return Hebrew)
                    month <- optionMaybe int
                    day   <- optionMaybe int
                    return $ DateSpec tp year month day

                     
timeSpec = do num <- int
              tp <- timeSpecType
              whiteSpace'
              return $ TimeSpec tp num
                
timeSpecType = option Days ((char 'y' >> return Years) <|>
                            (char 't' >> return Years360))

event = do whiteSpace'
           id <- mEventParticle
           eventSep
           text <- eventParticle
           eventSep
           date <- dateSpec
           try eof <|> lookAhead fakeNewline
           return $ Event id (strip text) date

eventSpec = (ESEvent <$> try event)
        <|> (ESId <$> try eventId)
        <|> (ESGroup <$> try (reserved' "group" >> eventId))
             
egroup = do mapM_ (reserved tokenParser) ["begin", "group"]
            name <- identifier tokenParser
            fakeNewline            
            events <- many (do res <- eventSpec
                               lexeme' fakeNewline
                               return res)
            mapM_ (reserved tokenParser) ["end", "group"]
            fakeNewline
            return $ TEGroup name events

topExp = try (TEEvent <$> event) <|> egroup

insertBefore x y (z:zs) | x == z    = y:x:insertBefore x y zs
                        | otherwise = z:  insertBefore x y zs
insertBefore _ _ [] = []

topLevel = do res <- many (lexeme' (do many (lexeme' fakeNewline)
                                       lexeme' topExp))
              eof
              return res

parseFile fp = do
  cont <- readFile fp
  let cont' = insertBefore '\n' fakeNewlineChar cont
  return $ parse topLevel fp cont'

type SimpleEvent = (String, HDate)
type EventList = [SimpleEvent]

toSimpleEventList :: [TopExp] -> [(GroupId, [SimpleEvent])]
toSimpleEventList exps = M.toList .
                         resolveGroups exps .
                         resolveDates .
                         registerEvents $ exps

  where  
    -- in the first pass we just id all the dates without trying
    -- to resolve the dates or events in a group
    registerEvents :: [TopExp] -> (M.Map EventId Event, M.Map GroupId [String])
    registerEvents tes = let (mes, mgs, _) = foldr idTopExp (M.empty, M.empty, 1) tes
                         in (mes, mgs)

    toId :: Int -> String
    toId i = printf "$e%d" i

    tagEvent :: Event -> Int -> (Event, Int)
    tagEvent e nextId = case eId e of
      Nothing -> (e { eId = Just $ toId nextId }, nextId+1)
      Just _ -> (e, nextId)

    saveEvent e nextId mes = 
      let (e', nextId') = tagEvent e nextId
      in (M.insert (fromJust.eId $ e') e' mes, nextId')

    idTopExp (TEEvent e) (mes, mgs, nextId) =
      let (mes', nextId') = saveEvent e nextId mes
      in (mes', mgs, nextId')
    idTopExp (TEGroup gid especs) (mes, mgs, nextId) =
      let es = groupEvents especs
          (mes', nextId') = foldr idGroup (mes, nextId) es
          taggedIds = [eid | Just eid <- map eId es]
          generatedIds = map toId [nextId..nextId'-1]
      in (mes', M.insert gid (taggedIds++generatedIds) mgs, nextId')
    
    idGroup e (mes, nextId) = saveEvent e nextId mes

    -- all events which are defined inside a group
    groupEvents especs = [e | ESEvent e <- especs]

    -- resolves relative date specs, so only DateSpec and
    -- DateSpecStartEnd remain
    resolveDates :: (M.Map EventId Event, M.Map a b) ->
                    (M.Map EventId SimpleEvent, M.Map a b)
    resolveDates (mes, mgs) =
      (M.map (\e -> (eText e, resolveDate [] mes (eDate e))) mes, mgs)
    
    resolveDate ids mes ds =

      case ds of        
        (DateSpec tp y m d) -> case (tp,y,m,d) of
          (Regular,y', Nothing, Nothing) -> hYear y'
          (Regular,y', Just m', Nothing) -> hMonth y' m'
          (Regular,y', Just m', Just d') -> hDay y' m' d'
          (Hebrew, y', Nothing, Nothing) -> heYear y'
          (Hebrew, y', Just m', Nothing) -> heMonth y' m'
          (Hebrew, y', Just m', Just d') -> heDay y' m' d'
        
        DateSpecStartEnd dt1 dt2 ->
          hTimeSpan (resolveDate ids mes dt1) (resolveDate ids mes dt2)
        
        DateSpecEventId eid ->
          if eid `elem` ids
          then error $ "resolving dates led to a circle involving events: " ++ intercalate (", ") ids
          else case M.lookup eid mes of
            Nothing -> error $ "Unknown event: " ++ eid
            Just e -> resolveDate (eid:ids) mes (eDate e)
          
        DateSpecAfter ts ds -> addTime ts (resolveDate ids mes ds)

        DateSpecExtend ts ds ->
          let date = resolveDate ids mes ds
          in case date of
            HTimeSpan dt1 dt2 -> HTimeSpan dt1 (addTime ts dt2)
            dt1 -> HTimeSpan dt1 (addTime ts dt1)            
    
    -- expects a resolved date, that is, only DateSpecStartEnd or DateSpec
    -- is allowed
    addTime :: TimeSpec -> HDate -> HDate
    addTime ts (HTimeSpan dt1 dt2) =
      hTimeSpan (addTime ts dt1) (addTime ts dt2)
    addTime (TimeSpec Days n)     hDate = snd $ after      ("",hDate) "" (fromIntegral n)
    addTime (TimeSpec Years n)    hDate = snd $ afterSolar ("",hDate) "" (fromIntegral n)
    addTime (TimeSpec Years360 n) hDate = snd $ after360   ("",hDate) "" (fromIntegral n)

    resolveGroups :: [TopExp] ->
                     (M.Map EventId SimpleEvent,
                      M.Map GroupId [EventId]) ->
                     M.Map GroupId [SimpleEvent]
    resolveGroups tops (mes, origMgs) =
      
      let topGroups = [g | g@(TEGroup _ _) <- tops]

          resolveEventIds :: TopExp -> M.Map GroupId [SimpleEvent] -> M.Map GroupId [SimpleEvent]
          resolveEventIds (TEGroup gid especs) mgs =
            M.insert gid ([assertE eid
                          | (ESId eid) <- especs] ++
                          [assertE genId
                          | genId <- fromJust $ M.lookup gid origMgs]) mgs

          resolveGroupIds :: TopExp -> M.Map GroupId [SimpleEvent] -> M.Map GroupId [SimpleEvent]
          resolveGroupIds (TEGroup gid especs) mgs =
            M.update (\es -> Just $ es ++
                             concat [assertG gid
                                    | (ESGroup gid) <- especs])
            gid mgs

          assertE e = case M.lookup e mes of
            Nothing -> error $ "unknown event: " ++ e
            Just e' -> e'
          assertG g = case M.lookup g origMgs of
            Nothing -> error $ "unknown group: " ++ g
            Just eids -> map assertE eids

          mgs' = foldr resolveEventIds M.empty topGroups
          mgs'' = foldr resolveGroupIds mgs' topGroups 

      in mgs''
