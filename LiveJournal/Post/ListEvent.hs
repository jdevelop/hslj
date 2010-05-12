module LiveJournal.Post.ListEvent (
    ListEvent(..),
    SelectType(..),
    MbDateTime,
    listEvents
)
where

import LiveJournal.Pair
import LiveJournal.Session
import LiveJournal.Common
import LiveJournal.Post.Event as PE
import Data.DateTime
import Data.Array
import Data.Maybe
import Text.Printf

type MbDateTime = Maybe DateTime

data SelectType =   Day DateTime | 
                    LastN Int MbDateTime | 
                    One String |
                    SyncItems DateTime

selectTypeToString :: SelectType -> String
selectTypeToString (Day _) = "day"
selectTypeToString (LastN _ _) = "lastn"
selectTypeToString (One _)= "one"
selectTypeToString (SyncItems _) = "syncitems"

data ListEvent = ListEvent { itemId, poster, anum, url :: String, postedAt :: DateTime, event :: Event }

listEvents :: Session -> String -> Maybe Int -> Bool -> Bool -> SelectType -> LineEndings -> Maybe String -> IO (Result [Pair])
listEvents session username truncate preferSubject noProps selectType lineEnding journal = do
    
    makeLJCall session pairs (Right)
    where
        pairs = concat [ [  makePair "mode" "getevents",
                            makePair "user" username,
                            makePair "selecttype" selectType',
                            makePair "lineendings" (PE.leMapping ! lineEnding)
                         ],
                         makePairBool1 "prefersubject" preferSubject,
                         makePairBool1 "noprops" noProps,
                         mbMakePairShowable "truncate" truncate,
                         mbMakePairShowable "usejournal" journal,
                         selectTypeData selectType
                        ]
        selectType' = selectTypeToString selectType


selectTypeData :: SelectType -> [Pair]
selectTypeData (Day wantedDay) = [ makePair "year" yearStr,
                             makePair "month" monthStr,
                             makePair "day" dayStr]
    where
        print2Digits = printf "%02d"
        (year, month, day, _, _, _) = toGregorian wantedDay
        yearStr = print2Digits $ fromIntegral year
        monthStr = print2Digits month
        dayStr = print2Digits day
selectTypeData (One itemId) = [makePair "itemid" itemId]
selectTypeData (LastN amount Nothing) = [ makePair "howmany" (show amount) ]
selectTypeData (LastN amount (Just beforedate)) = [ makePair "howmany" (show amount), 
                                                    makePair "beforedate" fmtDate]
    where
        fmtDate = formatDateTime "%Y-%m-%d %T" beforedate
selectTypeData (SyncItems datetime) = [ makePair "lastsync" fmtDate ]
    where
        fmtDate = formatDateTime "%Y-%m-%d %T" datetime
        

makePairBool1 :: String -> Bool -> [Pair]
makePairBool1 name True = [makePair name "1"]
makePairBool1 _ False = []

mbMakePairShowable :: ( Show a ) => String -> Maybe a -> [Pair]
mbMakePairShowable name = maybeToList . fmap ( makePair name . show )
