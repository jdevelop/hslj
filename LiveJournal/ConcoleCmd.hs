module ConsoleCmd where

import Data.ByteString.Char8 as BStr
import LiveJournal.Transport
import LiveJournal.Common
import LiveJournal.Error
import Data.URLEncoded
import Text.Regex.Posix
import Prelude as P

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

ban_list_from :: Session -> String -> String -> IO (Result [String])
ban_list_from Anonymous _ _ = return $ Right AuthRequired
ban_list_from session username target = do
    makeLJCall session [makePair "mode" "ban_list",
                        makePair "user" username,
                        makePair "command" cmdString] buildUserList
    where
        cmdString | target == "" = "ban_list"
                  | otherwise    = "ban_list from " ++ target
        buildUserList = Left . P.map ( BStr.unpack . value ) . P.filter ( (=~ "cmd_line_\\d+") . name ) 

ban_list :: Session -> String -> IO (Result [String])
ban_list session username = ban_list_from session username ""
