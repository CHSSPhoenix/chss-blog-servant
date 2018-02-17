{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Lib
  ( startApp
  , app
  ) where

import           Control.Monad.Error.Class (MonadError (..))
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Logger      (runStderrLoggingT)
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Digest.Pure.MD5
import           Data.Maybe
import           Data.String.Conversions   (cs)
import qualified Data.Text                 as Txt
import           Database.Persist
import           Database.Persist.Sqlite   (ConnectionPool, createSqlitePool,
                                            entityVal, insert, runMigration,
                                            runSqlPersistMPool, runSqlPool,
                                            selectFirst, selectList, (!=.),
                                            (==.), (>=.))
import           Models
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant

type API
   = "user" :> ReqBody '[ JSON] User :> PostCreated '[ JSON] (Key User) :<|> "user" :> Get '[ JSON] [User] :<|> "user" :> Capture "userId" (Key User) :> Get '[ JSON] (Maybe User)

makeApp :: FilePath -> IO Application
makeApp dbFile = do
  pool <- runStderrLoggingT $ do createSqlitePool (cs dbFile) 5
  runSqlPool (preProcessDB) pool
  return $ app pool
  where
    preProcessDB = do
      runMigration migrateAll
      insert $ User (cs "John") (cs "Doe") (cs "doe@mail.com") (cs "password") --TODO: to be deleted, this is just for testing purposes
      insert $ User (cs "Maria") (cs "Doe") (cs "doe@mail.com") (cs "password") --TODO: to be deleted, this is just for testing purposes
      insert $ User (cs "Jorge") (cs "Doe") (cs "doe@mail.com") (cs "password") --TODO: to be deleted, this is just for testing purposes
      return ()

startApp :: FilePath -> IO ()
startApp dbFile = run 8080 =<< makeApp dbFile

app :: ConnectionPool -> Application
app pool = serve api $ server pool

api :: Proxy API
api = Proxy

server :: ConnectionPool -> Server API
server pool = createUserHandler pool :<|> getAllUsersHandler pool :<|> getUserWithId pool

createUserHandler :: ConnectionPool -> User -> Handler (Key User)
createUserHandler pool userDto = liftIO $ addUSer
  where
    addUSer =
      flip runSqlPersistMPool pool $ do
        let md5Digest = md5 $ (cs (userPassword userDto))
        let md5DigestText = cs (show md5Digest)
        let user = User (cs (userName userDto)) (cs (userLastName userDto)) (cs (userEmail userDto)) md5DigestText
        userId <- insert user
        return userId

getAllUsersHandler :: ConnectionPool -> Handler [User]
getAllUsersHandler pool = liftIO $ getAllUsers
  where
    getAllUsers :: IO [User]
    getAllUsers =
      flip runSqlPersistMPool pool $ do
        users <- selectList [] []
        return $ map entityVal users

getUserWithId :: ConnectionPool -> (Key User) -> Handler (Maybe User)
getUserWithId pool userId = liftIO $ getUser
  where
    getUser :: IO (Maybe User)
    getUser =
      flip runSqlPersistMPool pool $ do
        maybeUser <- get userId
        case maybeUser of
          Nothing -> return Nothing -- TODO: review how to directly do: throwError err404
          Just uu -> return $ Just uu
