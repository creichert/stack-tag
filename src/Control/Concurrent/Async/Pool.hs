
-- | Simple implementation for limiting the number
-- of active threads during concurrent computations
-- using a semaphore.

module Control.Concurrent.Async.Pool
       (
         mapPool
       , mapCapabilityPool
       ) where

import qualified Control.Exception as E
import Control.Concurrent
import Control.Concurrent.Async

-- ifdef GHC
-- import GHC.Conc (getNumProcessors)
-- endif


-- | Map async using 'getNumCapabilities' to determine
-- the number of active threads.
--
-- This function is a bit misleading as it doesn't actually utilize
-- 'forkOn' or exploit any control over whether the threads are
-- spread across physical processors. It does, however, provide a
-- nice starting point for most of the threads used in this program
-- which are heavily IO bound.
mapCapabilityPool :: Traversable t => (a -> IO b) -> t a -> IO (t b)
mapCapabilityPool f xs = do
  -- num <- getNumProcessors
  num <- getNumCapabilities
  mapPool (num+1) f xs

-- | Limit the number of threads which can be active at any
-- given time when using 'mapConcurrently'. The downside is
-- that this function will allocate all threads at once.
mapPool :: Traversable t => Int -> (a -> IO b) -> t a -> IO (t b)
mapPool num f xs = do
  sem <- newQSem num
  mapConcurrently (withQSem sem . f) xs

withQSem :: QSem -> IO a -> IO a
withQSem m = E.bracket_ (waitQSem m) (signalQSem m)
