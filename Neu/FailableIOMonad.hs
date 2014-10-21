module Neu.FailableIOMonad(throwError, FailableIO, io, failableIO) where

import Control.Monad.Error
import Neu.AST

newtype FailableIO a = FailableIO { runFailableIO :: ErrorT NeuError IO a }
    deriving (Monad, MonadIO, Functor, MonadError NeuError)

io :: IO a -> FailableIO a
io = liftIO

failableIO = runErrorT . runFailableIO

