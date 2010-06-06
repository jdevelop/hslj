module LiveJournal.Entity where

import LiveJournal.Error

newtype Result a = Result (Either LJError a)

makeErrorStr ::  String -> Result (Either LJError a)
makeErrorStr = Result . Left . SimpleError

makeError ::  LJError -> Result a
makeError = Result . Left

makeResult :: a -> Result a
makeResult = Result . Right
