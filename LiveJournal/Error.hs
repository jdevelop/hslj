module LiveJournal.Error (
    Error(..)
) where

data Error = AuthRequired |
             WrongCredentials |
             NoChallenge |
             WrongResponseFormat | 
             SimpleError { message :: String }
