{-# LANGUAGE DeriveGeneric #-}

module UniversalLanguageInterface (

    exportAndStart',
    exportAndStart,
    callInterpreter,

) where


import System.Console.ArgParser
import Data.ByteString (hGetLine, hPutStr)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.ByteString.Lazy.Char8 (unpack)
import GHC.Generics
import System.IO (openFile, hClose, IOMode(..))
import System.Posix.IO (openFd, closeFd, OpenFileFlags(..), fdRead, fdWrite, OpenMode(..))
import System.Directory (removeFile)
import Data.Aeson
import qualified Data.Map as M

-- caller

import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (createNamedPipe, unionFileModes, ownerReadMode, ownerWriteMode)
import System.Process (waitForProcess, createProcess, proc)

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
instance FromJSON NormalResult
instance FromJSON ErrorResult

instance ToJSON InputFunctionCall
instance FromJSON InputFunctionCall

flags = OpenFileFlags False False False False False
    
exportAndStart :: M.Map String (String -> IO String) -> IO ()
exportAndStart fs = 
    let parser :: ParserSpec CommandLineOptions
        parser = CommandLineOptions 
            `parsedBy` reqFlag "mode"
            `andBy` reqFlag "input-pipe"
            `andBy` reqFlag "output-pipe"
    in withParseResult parser (\options -> do
            hin <- openFile (inputPipe options) ReadMode
            hout <- openFd (outputPipe options) WriteOnly Nothing flags
            inputLine <- hGetLine hin 
            case decode (fromStrict inputLine) of 
                Nothing -> error "Unable to parse input " -- TODO, throw 500 onto hout
                Just (InputFunctionCall name args) -> 
                    case M.lookup name fs of 
                        Nothing -> error "Function name does not exist" -- TODO throw 500 to hout
                        Just f -> do 
                            message <- f args
                            fdWrite hout $ (unpack $ encode (NormalResult 200 message)) ++ "\n"

            

            hClose hin
            closeFd hout
    )
            

callInterpreter :: String -- interpreter name
                -> [String] -- arguments, typically program file path, in case of compiled program, [] 
                -> String -- function name
                -> String -- argument
                -> IO String -- result

callInterpreter intname progfile funname arg = 
    withSystemTempDirectory "haskellcallertemp" $ \tempDirPath -> do
        let callerToCalleePipe = tempDirPath ++ "/inp"
            calleeToCallerPipe = tempDirPath ++ "/outp"
        createNamedPipe callerToCalleePipe (ownerReadMode `unionFileModes` ownerWriteMode)
        createNamedPipe calleeToCallerPipe (ownerReadMode `unionFileModes` ownerWriteMode)
        (_, _, _, h) <- createProcess (proc intname $ progfile ++ ["--mode", "single", "--input-pipe", callerToCalleePipe, "--output-pipe", calleeToCallerPipe])
        hCallerToCallee <- openFd callerToCalleePipe WriteOnly Nothing flags
        fdWrite hCallerToCallee $ (unpack $ encode (InputFunctionCall funname arg)) ++ "\n"
        hCalleeToCaller <- openFile calleeToCallerPipe ReadMode
        -- hClose hCallerToCallee
        waitForProcess h
        line <- hGetLine hCalleeToCaller
         
        -- clean up
        hClose hCalleeToCaller
        closeFd hCallerToCallee
        -- probably fine without this, temp directory will be deleted
        removeFile callerToCalleePipe
        removeFile calleeToCallerPipe

        case decode (fromStrict line) of 
            Nothing -> case decode (fromStrict line) of
                            Nothing -> error "Unable to parse return"
                            Just (ErrorResult _ message) -> error message
            Just (NormalResult _ returnVal) -> return returnVal


    
        

    


