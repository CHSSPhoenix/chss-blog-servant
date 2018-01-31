{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    , app
    ) where

import           Control.Monad.IO.Class   (liftIO)
import           Control.Monad.Logger     (runStderrLoggingT)
import           Data.Aeson
import           Data.Aeson.TH
import           Data.String.Conversions  (cs)
import           Database.Persist
import           Database.Persist.Sqlite  (ConnectionPool, createSqlitePool,
                                           entityVal, insert, runMigration,
                                           runSqlPersistMPool, runSqlPool,
                                           selectFirst, selectList, (==.),
                                           (>=.))
import           Models
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant

type API = "users" :> Get '[JSON] [User]

makeApp :: FilePath -> IO Application
makeApp dbFile
  = do pool <- runStderrLoggingT $ do createSqlitePool (cs dbFile) 5
       runSqlPool (preProcessDB) pool
       return $ app pool
  where preProcessDB = do runMigration migrateAll
                          insert $ User (cs "John Doe") 30  --TODO: to be deleted, this is just for testing purposes
                          insert $ User (cs "Mark Trude") 25  --TODO: to be deleted, this is just for testing purposes
                          insert $ User (cs "Maria Db") 20  --TODO: to be deleted, this is just for testing purposes
                          return ()

startApp :: FilePath -> IO ()
startApp dbFile
  = run 8080 =<< makeApp dbFile

app :: ConnectionPool -> Application
app pool = serve api $ server pool

api :: Proxy API
api = Proxy

server :: ConnectionPool -> Server API
server pool = getAllUsersHandler
  where getAllUsersHandler :: Handler [User]
        getAllUsersHandler = liftIO $ getAllUsers

        getAllUsers :: IO [User]
        getAllUsers = flip runSqlPersistMPool pool $ do
            users <- selectList [UserAge >=. 0] []
            return $ map entityVal users
