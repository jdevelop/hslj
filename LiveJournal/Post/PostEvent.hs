module LiveJournal.Post.PostEvent where

import LiveJournal.Post.Event
import LiveJournal.Common
import LiveJournal.Session
import LiveJournal.Error
import LiveJournal.Pair as P
import Data.ByteString.Char8 as BStr

postEntry :: Session -> String -> Event -> IO (Result (String))
postEntry session username post = do
    makeLJCall session pairs extractEntryURL
    where
        pairs = P.makePair "mode" "postevent" : 
                P.makePair "user" username : 
                event2pairs post

extractEntryURL :: [Pair] -> Result String
extractEntryURL  = extractEntryURL' . P.findPair "url"
    where
        extractEntryURL' Nothing = Left WrongResponseFormat
        extractEntryURL' (Just url) = Right . BStr.unpack $ url
