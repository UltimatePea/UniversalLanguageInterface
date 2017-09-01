module FunctionCallections where

testFunc :: String -> IO String
testFunc str = do
    putLine "[Haskell] Test Function"
    return $ str ++ "Callee :: Haskell"
