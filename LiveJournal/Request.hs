module LiveJournal.Request where

data LJRequestParam = RequestParam { name, value :: String }

data LJRequest = Request { params :: [LJRequestParam] }

makeRequest :: [(String, String)] -> LJRequest
makeRequest = Request . fmap ( uncurry RequestParam )

makeRequestParams :: [(String, String)] -> [LJRequestParam]
makeRequestParams = fmap ( uncurry RequestParam )
