{-# LANGUAGE NoMonomorphismRestriction,FlexibleContexts#-}

module LiveLournal.ResponseParser where

import Text.Parsec as TP
import Control.Monad as CM
import Data.Map as DM
import Data.List as DL

type ObjectFactory b = String -> b
type ObjectUpdater b = String -> String -> b -> b
type ParseResult a b = (DM.Map String String, DM.Map String [a], DM.Map String ( DM.Map Int b ) )

data ResponseParserState a b = RPS { simpleMap :: DM.Map String String,
                                     listMap :: DM.Map String [a],
                                     objectMap :: DM.Map String (DM.Map Int b),
                                     newObjectF :: ObjectFactory b,
                                     updateObjectF :: ObjectUpdater b
                                   }

notNewlineP = TP.noneOf "\r\n"

enumeratedParser :: (Stream s m Char) => ParsecT s (ResponseParserState String b) m ()
enumeratedParser = do 
    paramName <- TP.many (TP.noneOf "_")
    TP.char '_'
    TP.many TP.digit
    TP.newline
    paramValue <- TP.many notNewlineP
    TP.optional TP.newline
    currentState <- TP.getState
    let listMap' = DM.alter ( updateMapKey paramValue ) paramName $ listMap currentState
    TP.putState $ currentState { listMap = listMap' }
    where
        updateMapKey value (Just params) = Just (value:params)
        updateMapKey value Nothing = Just [value]

primitiveParser :: (Stream s m Char) => ParsecT s (ResponseParserState a b) m ()
primitiveParser = do
    paramName <- TP.many notNewlineP
    TP.newline
    paramValue <- TP.many notNewlineP
    TP.optional TP.newline
    currentState <- TP.getState
    let simpleMap' = DM.insert paramName paramValue $ simpleMap currentState
    TP.putState $ currentState { simpleMap = simpleMap' }

objectParamParser :: (Stream s m Char) => ParsecT s (ResponseParserState a b) m ()
objectParamParser = do
    objectType <- TP.many (TP.noneOf "_")
    TP.char '_'
    objectId <- CM.liftM ( read :: String -> Int ) (TP.many TP.digit)
    TP.char '_'
    propertyName <- TP.many notNewlineP
    TP.newline
    propertyValue <- TP.many notNewlineP
    TP.optional TP.newline
    currentState <- TP.getState
    let tmpMap = objectMap currentState
        objectMap' = DM.alter ( updateMapKey currentState objectType objectId propertyName propertyValue ) objectType tmpMap
    TP.putState $ currentState { objectMap = objectMap' }
    where
        updateMapKey (RPS _ _ _ newObject updateObject) 
                     objectType objectId propertyName propertyValue 
                     Nothing = Just (DM.singleton objectId (updateObject propertyName propertyValue
                            (newObject objectType)))
        updateMapKey (RPS _ _ _ newObject updateObject) 
                     objectType objectId propertyName propertyValue 
                     (Just objTypeMap) = 
                        Just $ DM.alter ( Just . updExistingObj ) 
                               objectId objTypeMap
                    where
                        newObjectInst = newObject objectType
                        updExistingObj Nothing = updateObject propertyName propertyValue newObjectInst
                        updExistingObj (Just obj) = updateObject propertyName propertyValue obj

responseParser =
    parseData <|> finishData
    where
        parseData = do
            try objectParamParser <|> ( try enumeratedParser <|> primitiveParser)
            responseParser
        finishData = do
            TP.eof
            (RPS simpleMap listMap objectMap _ _ ) <- TP.getState
            return (simpleMap, DM.map DL.reverse listMap, objectMap)

parseResponse :: (Stream s [] Char) =>ObjectFactory b-> ObjectUpdater b-> s-> Either ParseError (ParseResult String b)
parseResponse newObject updateObject = 
    head . TP.runPT responseParser (RPS DM.empty DM.empty DM.empty newObject updateObject) ""
