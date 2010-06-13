module LiveJournal.Entity where

import LiveJournal.Error
import Data.Either

newtype Result a = Result { getLJResult :: Either LJError a } deriving (Show)

makeErrorStr ::  String -> Result (Either LJError a)
makeErrorStr = Result . Left . SimpleError

makeError ::  LJError -> Result a
makeError = Result . Left

makeResult :: a -> Result a
makeResult = Result . Right
