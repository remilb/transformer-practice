{-# LANGUAGE TypeFamilies #-}
module Main where

import           System.IO
import           Control.Monad.IO.Class
import           State                          ( StateT(..) )
import           State.Class
import           Writer                         ( WriterT(..) )
import           Writer.Class
import           Except                         ( ExceptT(..) )
import           Except.Class
import           Reader                         ( ReaderT(..) )
import           Reader.Class
import           Trans

type StringReaderWriter a = ReaderT String (WriterT String IO) a

runStringReaderWriter srw r = runWriterT $ runReaderT srw r

askName :: (MonadWriter m, MonadIO m, W m ~ String) => (m String)
askName = do
    liftIO $ putStrLn "What is your name?"
    name <- liftIO $ getLine
    tell $ "Acquired name: " ++ name ++ "\n"
    return name

tellMeWhatsOutside
    :: (MonadReader r m, MonadWriter m, r ~ String, W m ~ String)
    => String
    -> m String
tellMeWhatsOutside name = do
    env <- ask
    tell "Contemplating answer: Should we lie?\n"
    tell "Decision acquired: No foma is truly harmless\n"
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




