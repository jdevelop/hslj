{-# LANGUAGE NoMonomorphismRestriction,FlexibleContexts#-}

module LiveLournal.ResponseParser where

import Text.Parsec as TP
import Control.Monad as CM
import Data.Map as DM
import Control.Applicative hiding ( (<|>) )

data ResponseParam = PrimitiveParam { pObjName, pObjValue :: String } |
                     EnumeratedParam { eObjName, eObjValue :: String, eObjId :: Int } |
                     ObjectParam { objType, objName, objValue :: String, objId :: Int } deriving (Show)

notNewlineP = TP.noneOf "\r\n"

enumeratedParser ::  (Stream s m Char) => ParsecT s u m ResponseParam
enumeratedParser = do 
    paramName <- TP.many (TP.noneOf "_")
    TP.char '_'
    paramId <- CM.liftM read (TP.many (TP.digit))
    TP.newline
    paramValue <- TP.many notNewlineP
    TP.newline
    return $ EnumeratedParam paramName paramValue paramId

primitiveParser :: (Stream s m Char) => ParsecT s u m ResponseParam
primitiveParser = do
    paramName <- TP.many notNewlineP
    TP.newline
    paramValue <- TP.many notNewlineP
    TP.newline
    return $ PrimitiveParam paramName paramValue

objectParamParser :: (Stream s m Char) => ParsecT s u m ResponseParam
objectParamParser = do
    objectType <- TP.many (TP.noneOf "_")
    TP.char '_'
    objectId <- CM.liftM read (TP.many (TP.digit))
    TP.char '_'
    propertyName <- TP.many notNewlineP
    TP.newline
    propertyValue <- TP.many notNewlineP
    TP.newline
    return $ ObjectParam objectType propertyName propertyValue objectId

responseParser :: (Stream s m Char) =>Map String String -> ParsecT s u m (Map String String)
responseParser initMap = do
    parseData <|> ( TP.eof >> return initMap )
    where
        parseData = do
            result <- (try objectParamParser) <|> ( (try enumeratedParser) <|> primitiveParser)
            responseParser $ updateMap result
        updateMap ( PrimitiveParam name value ) = DM.insert name value initMap
        updateMap _ = initMap
