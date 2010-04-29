module ConsoleCmd where

import LiveJournal.Transport
import LiveJournal.Common
import LiveJournal.Error
import Data.URLEncoded
import Control.Monad.Error

ban_set_from :: Session -> String -> String -> String -> IO ( Result () )
ban_set_from Anonymous _ _ _ = return $ Right AuthRequired
ban_set_from session owner target comm = do
    makeLJCall session [makePair "mode" "consolecommand",
                        makePair "user" owner,
                        makePair "command" target'] (\_ -> Left ())
    where
        defaultV = "ban_set " ++ target
        target' | comm == "" = defaultV
                | otherwise  = defaultV ++ " from " ++ comm

ban_set :: Session -> String -> String -> IO ( Result () )
ban_set session username target = ban_set_from session username target ""

ban_list :: Session -> String -> String -> IO (Result [String])
ban_list session username target = do
    makeLJCall session [makePair "mode" "ban_list",
                        makePair "user" username,
                        makePair "command" cmdString] buildUserList
    where
        cmdString | target == "" = "ban_list"
                  | otherwise    = "ban_list from " ++ target
        buildUserList _ = Left []
