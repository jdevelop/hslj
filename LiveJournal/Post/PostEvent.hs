module LiveJournal.Post.PostEvent where

import LiveJournal.Post.Event
import LiveJournal.Common
import LiveJournal.Session
import LiveJournal.Error
import LiveJournal.Pair as P
import Data.ByteString.Char8 as BStr
import Data.Maybe

postEntry :: Session -> String -> Maybe String -> Event -> IO (Result (String))
postEntry session username target post = do
    makeLJCall session pairs extractEntryURL
    where
        pairs = P.makePair "mode" "postevent" : 
                P.makePair "user" username : 
                event2pairs post ++ targetJr
        targetJr | target == Nothing = []
                 | otherwise = [P.makePair "usejournal" ( fromJust target )]

extractEntryURL :: [Pair] -> Result String
extractEntryURL  = extractEntryURL' . P.findPair "url"
    where
        extractEntryURL' Nothing = Left WrongResponseFormat
        extractEntryURL' (Just url) = Right . BStr.unpack $ url
