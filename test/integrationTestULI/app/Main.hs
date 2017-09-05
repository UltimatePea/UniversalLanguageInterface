module Main where

import Lib

-- considering this is the root directory of the project
-- We are running the projects

main :: IO ()
main = do
    putTitle "Try build and install all packages"
    buildAndInstallPackages
    putTitle "Integration Testing"
    --putSubTitle "Copying testing resources"
    --copyTestResources
    putSubTitle "Testing"
    test
    

putTitle :: String -> IO ()
putTitle title = do
    putStrLn title
    putStrLn "=========================="

putSubTitle :: String -> IO ()
putSubTitle title = do
    putStrLn title
    putStrLn "======"
