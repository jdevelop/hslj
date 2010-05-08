module LiveJournal.Session where

data Session = Anonymous | Authenticated { password :: String }

instance Show Session where
    show Anonymous = "Anonymous session"
    show (Authenticated password) = "Authenticated by password"
