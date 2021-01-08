{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module MVarClock where

import Data.Time.Clock
import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import FRP.Rhine

type EventMVarT event m = ReaderT (MVar event) m

withEventMVar :: MVar event -> EventMVarT event m a -> m a
withEventMVar = flip runReaderT

withEventMVarS :: Monad m => MVar event -> ClSF (EventMVarT event m) cl a b -> ClSF m cl a b
withEventMVarS = flip runReaderS_

data MVarClock event = MVarClock

instance Semigroup (MVarClock event) where
    (<>) _ _ = MVarClock

instance MonadIO m => Clock (EventMVarT event m) (MVarClock event) where
    type Time (MVarClock event) = UTCTime
    type Tag (MVarClock event) = event
    initClock _ = do
        initialTime <- liftIO getCurrentTime
        return
            ( constM $ do
                mvar  <- ask
                event <- liftIO $ takeMVar mvar
                time  <- liftIO $ getCurrentTime
                return (time, event)
            , initialTime
            )

instance GetClockProxy (MVarClock event)

mVarClockOn :: MonadIO m => MVar event -> HoistClock (EventMVarT event m) m (MVarClock event)
mVarClockOn mvar = HoistClock
    { unhoistedClock = MVarClock
    , monadMorphism = withEventMVar mvar
    }

