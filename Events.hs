module Events where

import Dates
import HebrewCal

dIsraelDest = ("israel and syria destroyed", hYear $ -720)
dSennach = ("siege of sennacherib - judah almost falls", hYear $ -701)
dDownfallAssyria = ("downfall of assyria", hTimeSpan (hYear $ -626) (hYear $ -605))

-- third year of jojakim, when 609 was accession year
dFirstSiegeNebu = ("daniel kidnapped", heYear $ -606)

-- german wikipedia
dNebuAcc = ("accession of nebukadnezar", hYear $ -605)

-- might also be in 611 or 613
dNineveh = ("nineveh sacked", hYear (-612))

-- wikipedia gives "summer" of 605, i'll assume between june and august
dFallAssyria = ("assyria falls (battle of charchemish)", 
             hTimeSpan (hMonth (-605) 6) (hMonth (-605) 8))

-- nebukadnezar's firstYear starts 1st nisannu 604,
-- which is between 7th march and 19th april (german wikipedia)
dNebuFirst = ("nebukadnezars first year starts",
              hTimeSpan
              (hDay (-604) 3 7)
              (hDay (-604) 4 19))

dNebuDream = extend (after dNebuFirst "" 360) 
           "nebukadnezars dream of the standing picture" 360

-- wikipedia gives the exact date!?
-- according to the babylonian chronicles
-- http://en.wikipedia.org/wiki/Jehoiakim
dJehoiachinSiege = ("jehoiachin deported", hDay (-597) 3 16)

-- nineteenth year of nebukadnezar
dLastSiegeNebu = ("jerusalem destroyed", hYear $ -587)

dTempleDestroyed1 = ("temple destroyed", heDay (-587) 5 9)
dTempleDestroyed2 = ("alternative date temple dest.", heDay (-586) 5 9)

-- after belsazar died, his father nabonid reigned for another 4 years.
-- this is a bit guesswork, though.
dWritingWall = ("the writing on the wall", hYear (-543))

-- nabonid chronic says that the army entered babylon tishri 16th
-- kyros some 17 days later.
dFallBabel = ("babylon falls", heDay (-539) 7 16)
dKyrosEntersBabel = after dFallBabel "kyros enters babylon" 21 

-- kyros was king of persia already when he conquered babylon, so we
-- don't have to count accession years. his first year would start 
-- immediatetely
dFirstYearKyros = extend dKyrosEntersBabel "kyros' edict" 360

-- daniel receives revelation in dan 10 in this year
-- this is the third year kyros reigns in babylon;
-- he was king of persia for some years already.
dThirdYearKyros = afterSolar dFirstYearKyros "daniel sees jesus" 1

dSerubbabel = after dFirstYearKyros "serubbabel leaves babylon" 0

dStartSac1 = ("start of sacrifices if in same year", heDay (-538) 7 1)
dStartSac2 = ("start of sacrifices if in next year", heDay (-537) 7 1)

-- Now in the second year after their coming to the house of God at Jerusalem, in the second month, (ezr 3)
dTempleStart1 = ("temple foundations", heDay (-537) 2 1)
-- it may have been one year later though, or could they do the journey so quickly?
dTempleStart2 = ("temple foundations (alternative)", heDay (-536) 2 1)

dDiscourage1 = ("people of the land stop the building (speculative)", hMonth (-537) 6)
dDiscourage2 = ("people of the land stop the building (alternative, speculative)", hMonth (-536) 6)

dTempleRestart = ("build temple", hYear $ -520)
dTempleFinish = ("finish temple", heDay (-516) 12 3)

dAfterTemplePassah = ("passah after temple finished", heDay (-515) 1 14)

dEsraLeave = ("esra leaves babylon", heDay (-458) 1 1) -- hDay (-458) 4 8)
dEsraArrive = ("esra arrives in jerusalem", heDay (-458) 5 1) -- hDay (-458 8 4)
-- some scholars think ezra's artaxerxes was the 2nd
dEsraLeaveAlt = ("esra leaves babylon (artaxerxes II theory)", hDay (-397) 3 24)
dEsraArriveAlt = ("esra arrives in jerusalem (artaxerxes II theory)", heDay (-397) 5 1) -- (hDay (-397) 7 20)

-- if artaxerxes ii's rule also started in summer, then his 7th year would not end before
-- summer 396
dEsraLeaveAltAlt = ("esra leaves babylon (artaxerxes II theory, alt)", heDay (-396) 1 1)
dEsraArriveAltAlt = ("esra arrives in jerusalem (artaxerxes II theory, alt)", heDay (-396) 5 1)
dNehemia1 = ("nehemia leaves babylon (445)", heMonth (-445) 1) --  hTimeSpan (hDay (-445) 3 15) (hDay (-445) 4 12))

-- this is one month earlier than in torahcalendar.com, which assumes that the 
-- thirteenth month of 445 was counted first of 444 for some reason.
-- i am doing this solely because i want the result to match my theory (69 weeks
-- later being nisan 33), so it is dishonest of course; still, it might be possible
-- and seems to be the only possibility to reach a nisan with passah on the right day
-- of week.
-- however, if this was adar 2 and the persian year started only the month after,
-- then this explains why nehemiah still gives the 20th year of artaxerxes, when it
-- would be the 21st if the new year had already started.
-- then for some reason nehemiah would regard adar 2 as nisan in his own reckoning
-- while officially nisan was only the next month
-- another explanation is that artaxerxes' regnal years spanned summer - summer when
-- he ascended the throne in summer. then -465 (summer) - -464 would be his ascension
-- year, starting his first year in summer -464 and his 2th in summer -445, so 
-- spring -444 would still be his 20th year.
dNehemia2 = ("nehemia leaves babylon (alt., 444)", heDay (-445) 13 1) -- hTimeSpan (hDay (-444) 3 4) (hDay (-444) 4 2))

d7WeeksNehem1 = after dNehemia1 "7 360-weeks after artaxerxes' edict (starting 445)" (49*360)
d7WeeksNehem2 = after dNehemia2 "7 360-weeks after artaxerxes' edict (starting 444)" (49*360)
d69WeeksNehem1 = after dNehemia1 "69 360-weeks after artaxerxes' edict (starting 445)" (69*7*360)
d69WeeksNehem2 = after dNehemia2 "69 360-weeks after artaxerxes' edict (starting 444)" (69*7*360)

dAndersonStart = ("anderson's start date", hDay (-445) 3 14)
dAndersonEnd = ("anderson's end date", hDay 32 4 6)
dAnderson70 = after dAndersonStart "69 weeks after anderson's start date" (69*7*360)

dDareios = ("dareios rises to power", hMonth (-522) 9)

dXerxesStart = ("xerxes/ahasveros beginning reign (ester)", hYear (-486))
dXerxesEnd = ("xerxes/ahasveros death", hMonth (-465) 8)
-- seventh year + 1 year ascension
dEstherQueen = ("esther becomes queen",
                heMonth (-479) 10)
dPurimLots = ("purim lots cast before haman (est 3,7)",
              heDay (-474) 1 1)
dPurimRevenge = ("haman tries to destroy jews, but jews take revenge (purim)",
                 hTimeSpan (heDay (-474) 12 13) (heDay (-474) 12 15))
dHamansLaw = ("haman's law to destroy jews signed", heDay (-474) 1 13)
                                                        
                 
babelDates =
  [ dIsraelDest
  , dSennach
  -- , dDownfallAssyria
  , dFirstSiegeNebu
  , after dFirstSiegeNebu "70*360 after daniel kidnapped" (offsetCaptivity*360)
  , afterSolar dFirstSiegeNebu "70 solar after daniel kidnapped" offsetCaptivity
  , dNebuAcc
  , dNineveh
  , dFallAssyria
  , dNebuFirst
  , dNebuDream
  , dJehoiachinSiege
  , after dJehoiachinSiege "70*360 after jehoiachin" (offsetCaptivity*360)
  , afterSolar dJehoiachinSiege "70 after jehoiachin" offsetCaptivity
--  , dLastSiegeNebu
  , dTempleDestroyed1
  , dTempleDestroyed2
  , after dTempleDestroyed1 "70*360 after temple destr." (offsetCaptivity*360)
  , after dTempleDestroyed2 "70*360 after temple destr. (alt.)" (offsetCaptivity*360)
  , afterSolar dTempleDestroyed1 "70 solar after temple destr." offsetCaptivity
  , afterSolar dTempleDestroyed2 "70 solar after temple destr. (alt.)" offsetCaptivity
  , ("nebukadnezar's death", hYear (-562))
--  , ("nebukadnezar's madness ??? (just speculating...)", 
--      hTimeSpan (hYear (-583)) (hYear (-576)))
  , dWritingWall
  , dFallBabel
  , dKyrosEntersBabel
  , dFirstYearKyros
  , dSerubbabel
  , dThirdYearKyros
  , dStartSac1
--  , dStartSac2
  , dTempleStart1
--  , dTempleStart2
  , dDiscourage1
--  , dDiscourage2
--  , dDareios
  , dTempleRestart
  , dTempleFinish
  , dAfterTemplePassah
  , dEsraLeave
  , dEsraArrive
  , dEsraLeaveAlt
  , dEsraArriveAlt
  , dEsraLeaveAltAlt
  , dEsraArriveAltAlt
  , dNehemia1
  , dNehemia2
  , d7WeeksNehem1
  , d7WeeksNehem2
  , d69WeeksNehem1
  , d69WeeksNehem2
  , dXerxesStart
  , dXerxesEnd
  , dEstherQueen
  , dPurimLots
  , dPurimRevenge
  , dHamansLaw
  , dAndersonStart
  , dAndersonEnd
  , dAnderson70
  ]
  
dates360 =
  [ ("seven years war ends; prussia rises to great power", hYear 1763)
  , ("sugar act, stamp act, resistance", hTimeSpan (hYear 1764) (hYear 1765))
  , ("american independence war peace ratified", hMonth 1784 1)
  , ("petah tikvah founded", hDay 1878 11 3)
  , ("france defeated, germany unified", hDay 1871 5 10)
  , ("first zionist congress", hTimeSpan (hDay 1897 8 29) (hDay 1897 8 31))
  , ("palestine immigration stop", hDay 1946 8 12)
  , ("exodus1947", hDay 1947 7 17)    
  , ("un partition plan", (hDay 1947 11 29))
  , ("declaration of independence", (hDay 1948 5 14))
  , ("start of independence war", (hDay 1948 5 15))
  , ("raising of the ink flag", hDay 1949 3 10)
  , ("egypt armistice", hDay 1949 2 24)
  , ("lebanon armistice", hDay 1949 3 23)
  , ("jordan armistice", hDay 1949 4 3)
  , ("syria armistice (last)", hDay 1949 7 20)
  , ("6 day war", hTimeSpan (hDay 1967 6 5) (hDay 1967 6 10))
  , ("founded jqdc", hDay 1969 4 1)
  , ("saddam sentenced to death", hDay 2006 11 5)
    -- arab spring events
  , ("arab spring starts in tunisia", hDay 2010 12 17)
  , ("tahrir square - egypt revolution starts", hDay 2011 1 25)
  , ("tunisian government overthrown", hDay 2011 1 14)
  , ("syrian protests start", hDay 2011 3 15)
  , ("mubarak resigns", hDay 2011 2 11)
  , ("lybian civil war starts", hDay 2011 2 15)
  , ("gaddafi overthrown", hDay 2011 8 28)
  , ("egyptian christians killed", hDay 2011 10 10)
  , ("gaddafi killed", hDay 2011 10 20)
  , ("syrian army attacks homs", hDay 2012 2 3)
    -- 1999 events
  , ("putin becomes prime minister", hDay 1999 8 9)
  , ("operation desert fox (iraq)", hTimeSpan (hDay 1998 12 16) (hDay 1998 12 20))
  , ("operation allied force (serbia)", hTimeSpan (hDay 1999 3 24) (hDay 1999 6 10))
    -- aliyah events
--  , ("1st aliyah", hTimeSpan (hYear 1882) (hYear 1903))
  , ("operation ezra & nehemiah (iraq)", hTimeSpan (hMonth 1951 5) (hMonth 1952 1))
  , ("operation magic carpet (yemen)", hTimeSpan (hMonth 1949 6) (hMonth 1950 9))
  , ("suez crisis, peak exodus from egypt", hTimeSpan (hDay 1956 10 29) (hDay 1956 11 2))

   -- ben yehuda
   , ("ben yehuda arrives in jaffa", hMonth 1881 10)
   , ("'ben zion' born", hYear 1882)
   , ("committee of hebrew founded", hYear 1890)
  ] -- ++ days100

days100 =
  let dates = takeWhile (<= hYear 2025) $ iterate (addDaysH 100) (hDay 1967 6 7)
      added = [0,100..]
  in zipWith (\d x -> ("added " ++ show d ++ " days",x)) added dates

datesSolar =
  [ ("napoleon first konsul", hDay 1799 12 25)
  , ("napoleon in egypt", hYear 1801)
  , ("napoleon emperor", hDay 1804 12 2)
  , ("iraq invasion", hTimeSpan (hDay 2003 3 19) (hDay 2003 4 30))
  , ("lebanon war", hYear 2006)
  , ("world war I; ottoman empire falls; jerusalem liberated; german empire falls",
     hTimeSpan (hDay 1914 7 28) (hDay 1918 11 11))
  , ("world war II; attempted eradication of jews;",
     hTimeSpan (hDay 1939 9 1) (hDay 1945 9 3))
  , ("treaty of versailles", hDay 1919 6 28)
  , ("hitler both president and chancellor", hDay 1934 8 1)
  , ("camp david accords", hDay 1978 9 17)
  , ("egypt peace treaty", hDay 1979 3 26)
  , ("saddam in power", hDay 1979 7 16)
  , ("oslo I", hDay 1993 8 20)
  , ("jordan peace treaty", hDay 1994 10 26)
  , ("oslo II", hDay 1995 9 24)
  , ("rabin's death", hDay 1995 11 4)
  , ("arafat's death", hDay 2004 11 11)
  , ("saddam's death", hDay 2006 12 30)
  , ("finished restoration of jewish quarter", hYear 1985)
  , ("operation moses (ethiopia)", hTimeSpan (hDay 1984 11 21) (hDay 1985 1 5))
  , ("operation joshua (ethiopia)", hMonth 1985 3)
  , ("operation salomon (ethiopia)", hTimeSpan (hDay 1991 5 23) (hDay 1991 5 25))
--  , ("2nd aliyah", hTimeSpan (hYear 1904) (hYear 1914))
--  , ("3rd aliyah", hTimeSpan (hYear 1919) (hYear 1923))
--  , ("4th aliyah", hTimeSpan (hYear 1924) (hYear 1931))
--  , ("5th aliyah", hTimeSpan (hYear 1932) (hYear 1938))
--  , ("aliyah B", hTimeSpan (hYear 1934) (hYear 1947))
   -- ben yehudah
   , ("hebrew official language", hYear 1921)
   , ("ben yehudah dies", hYear 1922)
  ]

datesModern = dates360 ++ datesSolar

dates360' =
  [ ("dome on the rock build start", hYear 685)
  , ("dome on the rock build end", hYear 691)
  , ("al-aqsa build end", hYear 705)
  , ("al-aqsa destroyed", hYear 746)
  , ("al-aqsa rebuilt", hYear 754)
  , ("mohammed's death", hYear 632)
  , ("mohammed born", hYear 570)
  ]
datesSolar' = dates360'

