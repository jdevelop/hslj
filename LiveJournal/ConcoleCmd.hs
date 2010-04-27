module ConsoleCmd where

import LiveJournal.Transport
import LiveJournal.Common
import LiveJournal.Error
import Data.URLEncoded
import Control.Monad.Error

ban_set_from :: Session -> String -> String -> String -> IO ( Result () )
ban_set_from Anonymous _ _ _ = return $ Right AuthRequired
ban_set_from session owner target comm = do
    response <- runRequestSession session [makePair "mode" "consolecommand",
                               makePair "user" owner,
                               makePair "command" target']
    return $ verifyResponse (responseStatus response) response
    where
        defaultV = "ban_set " ++ target
        target' | comm == "" = defaultV
                | otherwise  = defaultV ++ " from " ++ comm
        verifyResponse Nothing response = Right $ WrongResponseFormat
        verifyResponse (Just status) response | status == statusOk = Left ()
                                              | otherwise = Right $ getErrorMsgFromResponse response

ban_set :: Session -> String -> String -> IO ( Result () )
ban_set session username target = ban_set_from session username target ""

ban_list :: Session -> String -> String -> IO (Result [String])
ban_list session username target = do
    response <- runRequestSession session [makePair "mode" "ban_list",
                                           makePair "user" username,
                                           makePair "command" cmdString]
    return $ verifyResponse ( responseStatus response ) response
    where
        cmdString | target == "" = "ban_list"
                  | otherwise    = "ban_list from " ++ target
        verifyResponse Nothing _ = Right WrongResponseFormat
        verifyResponse (Just status) response | status == statusOk = Left $ buildUserList response
                                              | otherwise = Right $ getErrorMsgFromResponse response
        buildUserList _ = []
