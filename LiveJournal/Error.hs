module LiveJournal.Error (
    LJError(..)
) where

import Control.Monad.Error.Class

data LJError = AuthRequired |
             WrongCredentials |
             NoChallenge |
             WrongResponseFormat | 
             SimpleError { message :: String }

instance Error LJError
    where
        noMsg = WrongResponseFormat
        strMsg = SimpleError

instance Show LJError
    where
        show AuthRequired = "AuthRequired"
        show WrongResponseFormat = "WrongResponseFormat"
        show NoChallenge = "NoChallenge"
        show WrongCredentials = "WrongCredentials"
        show (SimpleError msg) = "SimpleError {" ++ msg ++ "}"
