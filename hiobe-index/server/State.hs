{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}

module State where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Reader
import Data.Map.Strict        (Map)
import Data.Map.Strict        qualified as Map
import Data.Text              (Text)

import Database.SQLite.Simple

newtype HiobeM a
  = HiobeM { runHiobeM :: ReaderT (TVar HiobeState) IO a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader (TVar HiobeState))

hiobeM :: MonadTrans t => HiobeM a -> t HiobeM a
hiobeM = lift

gets :: (HiobeState -> a) -> HiobeM a
gets g = asks readTVarIO >>= liftIO >>= return . g

modify :: (HiobeState -> HiobeState) -> HiobeM ()
modify g = ask >>= liftIO . atomically . flip modifyTVar g

modify' :: (HiobeState -> HiobeState) -> HiobeM ()
modify' g = ask >>= liftIO . atomically . flip modifyTVar' g

data HiobeState
  = HiobeState
    { dbConn          :: MVar Connection
    , reqCount        :: Map Text Integer
    , langEngagements :: Map Text Integer
    }

initState :: MVar Connection -> HiobeState
initState dbConn =
  HiobeState
  { dbConn          = dbConn
  , langEngagements = Map.empty
  , reqCount        = Map.empty
  }

putLang :: Text -> HiobeM ()
putLang l =
  modify $ \HiobeState{..} ->
    HiobeState dbConn reqCount (Map.insertWith (+) l 1 langEngagements)

putReq :: Text -> HiobeM ()
putReq p = do
  modify $ \HiobeState{..} ->
    HiobeState dbConn (Map.insertWith (+) p 1 reqCount) langEngagements
