{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module App where

import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Logger           ( runStderrLoggingT )
import           Data.String.Conversions        ( cs )
import           Data.Text                      ( Text )
import           Database.Persist.Sqlite        ( ConnectionPool
                                                , createSqlitePool
                                                , entityVal
                                                , insert
                                                , replace
                                                , runMigration
                                                , runSqlPersistMPool
                                                , runSqlPool
                                                , selectFirst
                                                , (==.)
                                                )
import           Database.Persist.Types         ( Entity(..) )
import           Network.Wai.Handler.Warp      as Warp

import           Servant

import           Api
import           Models

server :: ConnectionPool -> Server Api
server pool =
  fetchSisterHandler pool
    :<|> addSisterHandler pool
    :<|> updateSisterHandler pool

fetchSisterHandler :: ConnectionPool -> Text -> Handler Sister
fetchSisterHandler pool name = do
  maybeSister <- liftIO $ fetchSisterDB pool name
  case maybeSister of
    Just sister -> return sister
    Nothing     -> throwError $ err404 { errBody = "nah she ain't here b" }

fetchSisterDB :: ConnectionPool -> Text -> IO (Maybe Sister)
fetchSisterDB pool name = flip runSqlPersistMPool pool $ do
  mSister <- selectFirst [SisterName ==. name] []
  return $ entityVal <$> mSister

addSisterHandler :: ConnectionPool -> Sister -> Handler Text
addSisterHandler pool sister = do
  maybeSister <- liftIO $ addSisterDB pool sister
  case maybeSister of
    Just _  -> return "success"
    Nothing -> throwError $ err401 { errBody = "name taken b" }

addSisterDB :: ConnectionPool -> Sister -> IO (Maybe (Key Sister))
addSisterDB pool newSister = flip runSqlPersistMPool pool $ do
  exists <- selectFirst [SisterName ==. sisterName newSister] []
  case exists of
    Nothing -> Just <$> insert newSister
    Just _  -> return Nothing

updateSisterHandler :: ConnectionPool -> Text -> Sister -> Handler Text
updateSisterHandler pool name sister = do
  maybeSister <- liftIO $ updateSisterDB pool name sister
  case maybeSister of
    Just _  -> return "success"
    Nothing -> throwError $ err404 { errBody = "nah she ain't here b" }

updateSisterDB :: ConnectionPool -> Text -> Sister -> IO (Maybe ())
updateSisterDB pool name sister = flip runSqlPersistMPool pool $ do
  exists <- selectFirst [SisterName ==. name] []
  case exists of
    Nothing               -> return Nothing
    Just (Entity sisId _) -> Just <$> replace sisId sister

app :: ConnectionPool -> Application
app pool = serve api $ server pool

mkApp :: FilePath -> IO Application
mkApp sqliteFile = do
  pool <- runStderrLoggingT $ createSqlitePool (cs sqliteFile) 5
  runSqlPool (runMigration migrateAll) pool
  return $ app pool

run :: FilePath -> IO ()
run sqliteFile = Warp.run 8080 =<< mkApp sqliteFile
