module LiveJournal.ConsoleCmd where

import Data.ByteString.Char8 as BStr
import LiveJournal.Transport
import LiveJournal.Common
import LiveJournal.Error
import Data.URLEncoded
import Text.Regex.Posix
import Prelude as P

voidHandler :: LJResponseHandler ()
voidHandler = \_ -> Right ()

buildUserList :: [Pair] -> [String]
buildUserList = P.map ( BStr.unpack . value ) . P.filter ( (=~ "cmd_line_\\d+") . name ) 

runConsoleCommand :: Session -> String -> String -> LJResponseHandler a -> IO (Result a)
runConsoleCommand session owner command handler = do
    makeLJCall session [makePair "mode" "consolecommand",
                        makePair "user" owner,
                        makePair "command" command] handler

ban_set_from :: Session -> String -> String -> String -> IO ( Result () )
ban_set_from session owner target comm = runConsoleCommand session owner cmdString voidHandler
    where
        defaultV = "ban_set " ++ target
        cmdString | comm == "" = defaultV
                  | otherwise  = defaultV ++ " from " ++ comm

ban_set :: Session -> String -> String -> IO ( Result () )
ban_set session username target = ban_set_from session username target ""

ban_list_from :: Session -> String -> String -> IO (Result [String])
ban_list_from session owner target = runConsoleCommand session owner cmdString $ Right . buildUserList
    where
        cmdString | target == "" = "ban_list"
                  | otherwise    = "ban_list from " ++ target

ban_list :: Session -> String -> IO (Result [String])
ban_list session username = ban_list_from session username ""

ban_unset_from :: Session -> String -> String -> String -> IO (Result ())
ban_unset_from session owner target comm = runConsoleCommand session owner cmdString voidHandler
    where
        defaultV = "ban_unset " ++ target
        cmdString | target == "" = defaultV
                  | otherwise = defaultV ++ " from " ++ comm

ban_unset :: Session -> String -> String -> IO (Result ())
ban_unset session owner target = ban_unset_from session owner target ""

friend_mgmt :: Session -> String -> String -> String -> String -> String -> String -> LJResponseHandler a -> IO (Result a)
friend_mgmt session cmd owner username group fgcolor bgcolor handler = do
    runConsoleCommand session owner cmdString handler
    where
        cmdString = cmd ++ (P.concatMap prependSpace $ [username,group] ++ optArgs)
        optArgs = P.map ( makePair' ) . P.filter ( ("" /= ) . snd ) $ P.zip ["fgcolor","bgcolor"] [fgcolor,bgcolor]
        makePair' (name,value) = name ++ "='" ++ value ++ "'"
        prependSpace arg | arg == "" = ""
                         | otherwise = " " ++ arg

friend_add :: Session -> String -> String -> String -> String -> String -> IO (Result ())
friend_add session owner username group fgcolor bgcolor = friend_mgmt session "add" owner username group fgcolor bgcolor voidHandler

friend_list :: Session -> String -> String -> String -> String -> IO (Result [String])
friend_list session owner group fgcolor bgcolor = friend_mgmt session "list" owner "" group fgcolor bgcolor $ Right . buildUserList

friend_delete :: Session -> String -> String -> String -> String -> String -> IO (Result ())
friend_delete session owner username group fgcolor bgcolor = friend_mgmt session "remove" owner username group fgcolor bgcolor voidHandler
