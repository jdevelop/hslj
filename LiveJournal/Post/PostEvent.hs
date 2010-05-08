module LiveJournal.Post.PostEvent where

import LiveJournal.Post.Event
import LiveJournal.Common
import LiveJournal.Session
import LiveJournal.Pair

postEntry :: Session -> String -> Event -> IO (Result ())
postEntry session username post = do
    makeLJCall session pairs (\_ -> Right ())
    where
        pairs = event2pairs post
