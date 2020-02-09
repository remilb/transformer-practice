module Main where

import           System.IO
import           Control.Monad.IO.Class
import           State
import           Writer
import           Except
import           Reader
import           Trans

type StringReaderWriter a = ReaderT String (WriterT String IO) a

runStringReaderWriter srw r = runWriterT $ runReaderT srw r

askName :: StringReaderWriter String
askName = do
    liftIO $ putStrLn "What is your name?"
    name <- liftIO $ getLine
    lift $ tell $ "Acquired name: " ++ name ++ "\n"
    return name

tellMeWhatsOutside :: String -> StringReaderWriter String
tellMeWhatsOutside name = do
    env <- ask
    lift $ tell "Contemplating answer: Should we lie?\n"
    lift $ tell "Decision acquired: No foma is truly harmless\n"
    return $ "Well " ++ name ++ ", " ++ env


main :: IO ()
main =
    let env = "best stay inside, it's a real pool-pah out there!"
    in  do
            (log, response) <- runStringReaderWriter
                (askName >>= tellMeWhatsOutside)
                env
            putStrLn response
            putStrLn "\nLog:"
            putStrLn log
            return ()




