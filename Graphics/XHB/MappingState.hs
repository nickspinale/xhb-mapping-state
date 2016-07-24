{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}


module Graphics.XHB.MappingState
    ( MappingState(..)
    , MappingT(..)
    , runMappingT
    , MappingCtx(..)
    , getsMapping
    ) where


import Graphics.XHB
import Graphics.XHB.Monad
import Graphics.XHB.MappingState.Internal

import Data.Typeable

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer


newtype MappingT m a = MappingT { unMappingT :: StateT MappingState m a }
    deriving (Functor, Applicative, Monad, MonadIO, Typeable)

instance MonadTrans MappingT where
    lift = MappingT . lift

instance MonadX x m => MonadX x (MappingT m) where
    liftX = lift . liftX
    askX = lift askX
    catchErrorX m f = MappingT $ catchErrorX (unMappingT m) (unMappingT . f)


runMappingT :: MonadX x m => MappingT m a -> m a
runMappingT m = initMapState >>= evalStateT (unMappingT m)


-- Class --


class Monad m => MappingCtx m where
    getMapping :: m MappingState
    updateMapping :: MappingNotifyEvent -> m ()

instance MonadX x m => MappingCtx (MappingT m) where
    getMapping = MappingT get
    updateMapping ev = MappingT $ updateMapState ev >>= modify

instance (MappingCtx m, MonadTrans t, Monad (t m)) => MappingCtx (t m) where
    getMapping = lift getMapping
    updateMapping = lift . updateMapping


getsMapping :: MappingCtx m => (MappingState -> a) -> m a
getsMapping = flip fmap getMapping


-- mtl instances --


instance MonadError e m => MonadError e (MappingT m) where
    throwError = lift . throwError
    catchError (MappingT m) f = MappingT $ catchError m (unMappingT . f)

instance MonadReader r m => MonadReader r (MappingT m) where
    ask = lift ask
    local f = MappingT . local f . unMappingT

instance MonadState s m => MonadState s (MappingT m) where
    get = lift get
    put = lift . put

instance MonadWriter w m => MonadWriter w (MappingT m) where
    tell = lift . tell
    listen = MappingT . listen . unMappingT
    pass = MappingT . pass . unMappingT
