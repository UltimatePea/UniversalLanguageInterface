module HaskellPythonBridge where


class CallSetting setting where

class CallResult res where

callpython :: (CallSetting a, CallResult b) => a -> IO b
callpython = undefined
