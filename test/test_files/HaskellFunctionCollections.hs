module HaskellFunctionCollections where

testFunc :: String -> IO String
testFunc str = do
    -- putStrLn "[Haskell] Test Function"
    return $ str ++ "Callee :: Haskell"
