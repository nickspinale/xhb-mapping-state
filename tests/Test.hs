module Test where


import Graphics.XHB
import Graphics.XHB.Monad
import Graphics.XHB.MappingState

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer


get :: IO (Either SomeError MappingState)
get = do
    Just conn <- connect
    (unX . runMappingT $ getMapping) conn
