module LiveJournal.Entity where

import LiveJournal.Error
import Control.Monad.Error

type Result m a = ErrorT LJError m a

type IOResult a = Result IO a

makeErrorStr :: String -> IOResult a
makeErrorStr = ErrorT . return . Left . SimpleError

makeError :: (Monad m) =>  LJError -> Result m a
makeError = ErrorT . return . Left

makeResult :: (Monad m) => a -> Result m a
makeResult = ErrorT . return . Right
