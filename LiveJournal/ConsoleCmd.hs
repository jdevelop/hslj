module LiveJournal.ConsoleCmd where

import LiveJournal.Transport
import LiveJournal.Common
import LiveJournal.Error

runConsoleCommand :: Session -> String -> String -> LJResponseHandler a -> IO (Result a)
runConsoleCommand session owner command handler = do
    makeLJCall session [makePair "mode" "consolecommand",
                        makePair "user" owner,
                        makePair "command" command] handler
