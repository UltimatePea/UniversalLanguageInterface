{-# LANGUAGE DeriveGeneric #-}

module UniversalLanguageInterface (

    exportAndStart',
    exportAndStart

) where

import System.Console.ArgParser
import Data.ByteString (hGetLine, hPutStr)
import Data.ByteString.Lazy (fromStrict, toStrict)
import GHC.Generics
import System.IO (openFile, hClose, IOMode(..))
import Data.Aeson
import qualified Data.Map as M

exportAndStart' :: [(String, String -> IO String)] -> IO ()
exportAndStart' fs = exportAndStart $ M.fromList fs

data CommandLineOptions = CommandLineOptions {
    mode :: String,
    inputPipe :: String,
    outputPipe :: String } deriving (Show, Eq)

data InputFunctionCall = InputFunctionCall {
    name :: String,
    args :: String
} deriving (Show, Generic)

data NormalResult = NormalResult {
    ncode :: Int,
    return_val :: String
} deriving (Show, Generic)
    
data ErrorResult = ErrorResult {
    ecode :: Int,
    message :: String
} deriving (Show, Generic)

instance ToJSON NormalResult
instance ToJSON ErrorResult

instance FromJSON InputFunctionCall
    
exportAndStart :: M.Map String (String -> IO String) -> IO ()
exportAndStart fs = 
    let parser :: ParserSpec CommandLineOptions
        parser = CommandLineOptions 
            `parsedBy` reqFlag "mode"
            `andBy` reqFlag "input-pipe"
            `andBy` reqFlag "output-pipe"
    in withParseResult parser (\options -> do
            hin <- openFile (inputPipe options) ReadMode
            hout <- openFile (outputPipe options) WriteMode
            inputLine <- hGetLine hin 
            case decode (fromStrict inputLine) of 
                Nothing -> error "Unable to parse input " -- TODO, throw 500 onto hout
                Just (InputFunctionCall name args) -> 
                    case M.lookup name fs of 
                        Nothing -> error "Function name does not exist" -- TODO throw 500 to hout
                        Just f -> do 
                            message <- f args
                            hPutStr hout $ toStrict $ encode (NormalResult 200 message)

            

            hClose hin
            hClose hout
    )
            
        
        

    


