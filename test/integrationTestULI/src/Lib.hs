module Lib
    ( buildAndInstallPackages
    , test
    ) where

-- considering this is the root directory of the project
-- We are running the projects
-- For every command, restore the status 

import System.Directory
import System.Process
import Data.List.Split

a </> b = a ++ "/" ++ b

buildAndInstallPackages :: IO ()
buildAndInstallPackages = do
    curDir <- getCurrentDirectory
    -- note, individual actions must also restore the working directory
    putStrLn "Building python3"
    build (\x -> x </> "python3") curDir "python3 setup.py install"
    putStrLn "Building Haskell"
    build (\x -> x </> "haskell" </> "UniversalLanguageInterface") curDir "cabal install"
    putStrLn "Building NodeJs"
    build (\x -> x </> "nodejs") curDir "npm install -g"
    putStrLn "Done Building"
    setCurrentDirectory curDir
    

build :: (FilePath -> FilePath) -- how to modify current directory e.g. (</> "python3" </> "package")
      -> FilePath -- current directory
      -> String -- build command
      -> IO ()
build pathAppend curDir command = 
    let packagePath = pathAppend curDir
    in withCurrentDirectory packagePath $ do
            exec command
            return ()
    
exec :: String -> IO String
exec command = do
    let (progName:args) = splitOn " " command
    readProcess progName args ""

test :: IO ()
test = do
    curDir <- getCurrentDirectory
    setCurrentDirectory (curDir </> "test" </> "test_files" )
    -- haskell 
    execAssertEqual "runhaskell THaskellCallHaskell.hs" "Caller :: Haskell -- Callee :: Haskell\n"
    execAssertEqual "runhaskell THaskellCallPython3.hs" "Caller :: Haskell -- Callee :: python3\n"
    -- python3
    execAssertEqual "python3 T_python3_call_python3.py" "Caller :: python3 -- Callee :: python3\n"
    execAssertEqual "python3 T_python3_call_haskell.py" "Caller :: python3 -- Callee :: Haskell\n"
    -- node
    exec "npm link universal-language-interface" -- npm must use local imports, so we need to install this package here

    setCurrentDirectory curDir

execAssertEqual :: String -- command
                 -> String -- assertion 
                 -> IO () -- error on not equal
execAssertEqual cmd str = do
    ret <- exec cmd
    assert str ret

assert :: (Eq a, Show a) => a -> a -> IO ()
assert a b 
    | a == b = return ()
    | otherwise = error $ "Error, " ++ show a ++ " is not equal to " ++ show b
