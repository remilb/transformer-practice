module Main where

import           State
import           Writer
import Except
import Reader
import Trans

type StringWriter a = Writer String a

type IOException a = ExceptT String IO a

type StringReaderWriter a = ReaderT String (WriterT String IO) a

runStringReaderWriter srw r = runWriterT $ runReaderT srw r

askAndLog :: StringReaderWriter String
askAndLog = do 
    str <- ask
    lift $ tell "Fuckkkk"
    (a,s) <- lift $ listen (tell "Fuckk")
    return a

main :: IO ()
main = do
    (log, result) <- flip runStringReaderWriter "Environment" $ askAndLog
    
    print result

dontGiveMeFive :: Int -> IOException Int
dontGiveMeFive 5 = throwError "I said don't give me 5!"
dontGiveMeFive n = return n


-- addOne :: Int -> StringWriter Int
-- addOne n = Writer $ ("I added one!\n", n + 1)

-- subtractFive :: Int -> StringWriter Int
-- subtractFive n = Writer $ ("I subtracted 5! Woo!", n - 5)


