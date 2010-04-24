module LiveJournal.Auth where 

import Network.Curl

data Session = Anonymous | Authenticated { auth_challange, auth_response :: String }

login :: String -> String -> IO (  )
login username password = do
    curl <- initialize
    response <- do_curl curl "http://www.livejournal.com/interface/flat" method_POST
    putStrLn $ respBody response
