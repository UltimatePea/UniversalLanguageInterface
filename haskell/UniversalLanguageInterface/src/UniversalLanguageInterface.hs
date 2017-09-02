{-# LANGUAGE DeriveGeneric #-}

module UniversalLanguageInterface (

    exportAndStart',
    exportAndStart,
    callInterpreter,

) where


import System.Console.ArgParser
import Data.ByteString (hGetLine, hPutStr)
import Data.ByteString.Lazy (fromStrict, toStrict)
import GHC.Generics
import System.IO (openFile, hClose, IOMode(..))
import System.Directory (removeFile)
import Data.Aeson
import qualified Data.Map as M

-- caller

import System.IO.Temp (withSystemTempDirectory, createTempDirectory, getCanonicalTemporaryDirectory)
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
    
exportAndStart :: M.Map String (String -> IO String) -> IO ()
exportAndStart fs = 
    let parser :: ParserSpec CommandLineOptions
        parser = CommandLineOptions 
            `parsedBy` reqFlag "mode"
            `andBy` reqFlag "input-pipe"
            `andBy` reqFlag "output-pipe"
    in withParseResult parser (\options -> do

            putStrLn "Callee :: Opening Callee to Caller Pipe"
            hout <- openFile (outputPipe options) WriteMode
            putStrLn "Callee :: Done Opening Callee to Caller Pipe"

            putStrLn "Callee :: Opening Caller to Callee Pipe"
            hin <- openFile (inputPipe options) ReadMode
            putStrLn "Callee :: Done Opening Caller to Callee Pipe"


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
            

callInterpreter :: String -- interpreter name
                -> String -- program file path
                -> String -- function name
                -> String -- argument
                -> IO String -- result

callInterpreter intname progfile funname arg =  do
    -- withSystemTempDirectory "haskellcallertemp" $ \tempDirPath -> do
    -- try not using lazy io
    tempPath <- getCanonicalTemporaryDirectory
    tempDirPath <- createTempDirectory tempPath "haskellcallertemp"
    do
        let callerToCalleePipe = tempDirPath ++ "/inp"
            calleeToCallerPipe = tempDirPath ++ "/outp"
        createNamedPipe callerToCalleePipe (ownerReadMode `unionFileModes` ownerWriteMode)
        createNamedPipe calleeToCallerPipe (ownerReadMode `unionFileModes` ownerWriteMode)
        (_, _, _, h) <- createProcess (proc intname [progfile, "--mode", "single", "--input-pipe", callerToCalleePipe, "--output-pipe", calleeToCallerPipe])

        putStrLn "Caller :: Opening Callee to Caller Pipe"
        hCalleeToCaller <- openFile calleeToCallerPipe ReadMode
        putStrLn "Caller :: Done Opening Callee to Caller Pipe"

        putStrLn "Caller :: Opening Caller to Callee Pipe"
        hCallerToCallee <- openFile callerToCalleePipe WriteMode
        putStrLn "Caller :: Done Opening Caller to Callee Pipe"

        -- write one line to inp
        hPutStr hCallerToCallee $ toStrict $ encode (InputFunctionCall funname arg)


        -- hClose hCallerToCallee
        waitForProcess h
        line <- hGetLine hCallerToCallee
         
        -- clean up
        putStrLn "Caller Cleaning Up"
        hClose hCalleeToCaller
        hClose hCallerToCallee
        putStrLn "Caller Done Cleaning Up"
        -- probably fine without this, temp directory will be deleted
        --removeFile callerToCalleePipe
        --removeFile calleeToCallerPipe

        case decode (fromStrict line) of 
            Nothing -> case decode (fromStrict line) of
                            Nothing -> error "Unable to parse return"
                            Just (ErrorResult _ message) -> error message
            Just (NormalResult _ returnVal) -> return returnVal


    
        

    


