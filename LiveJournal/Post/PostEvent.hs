module LiveJournal.Post.PostEvent (
    postEvent,
    editEvent
) where

import LiveJournal.Post.Event
import LiveJournal.Common
import LiveJournal.Session
import LiveJournal.Error
import LiveJournal.Pair as P
import Data.ByteString.Char8 as BStr
import Data.Maybe
import Prelude as Pr

postEvent :: Session -> String -> Maybe String -> Event -> IO (Result (String))
postEvent session username target post = do
    makeLJCall session pairs extractEntryURL
    where
        pairs = P.makePair "mode" "postevent" : 
                P.makePair "user" username : 
                createEvent post target

editEvent :: Session -> String -> Maybe String -> String -> Event -> IO (Result (String))
editEvent session username target eventid post = do
    makeLJCall session pairs extractEntryURL
    where
        pairs = P.makePair "mode" "editevent" :
                P.makePair "user" username :
                P.makePair "eventid" eventid :
                createEvent post target

extractEntryURL :: [Pair] -> Result String
extractEntryURL  = extractEntryURL' . P.findPair "url"
    where
        extractEntryURL' Nothing = Left WrongResponseFormat
        extractEntryURL' (Just url) = Right . BStr.unpack $ url

targetJr :: Maybe String -> [Pair]
targetJr = maybeToList . fmap (P.makePair "usejournal")

createEvent :: Event -> Maybe String -> [Pair]
createEvent post target = Pr.concat [event2pairs post, targetJr target ]
