#!/usr/bin/env stack
-- stack script --resolver lts-9.14 --package async --package transformers --package deepseq

-- Fixes the issue in statet-concurrently-1.hs , but commits the abomination of
-- using unsafePerformIO. 'f' and 'g' can execute concurrently, but as soon as
-- 'g' requires the state, it blocks on the completion of 'f'.
--
-- Output:
--
-- > Before modification in g
-- > Before modification in f
-- > Done with modification in f
-- > Done with modification in g
-- > ["monad","transformers","!"]
--
-- As expected, execution is interspersed!

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.DeepSeq
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import System.IO.Unsafe

concurrentlyS :: StateT s IO a -> StateT s IO b -> StateT s IO (a, b)
concurrentlyS (StateT f) (StateT g) = StateT $ \s0 -> do
  v <- newEmptyMVar
  let f' = do
        (a, s) <- f s0
        putMVar v s
        return a
      s1 = unsafePerformIO (takeMVar v)
  (a, (b, s2)) <- concurrently f' (g s1)
  return ((a, b), s2)

main :: IO ()
main = do
  let f = do
        lift $ do
          threadDelay (10 * 1000)
          putStrLn "Before modification in f"
        modify' ("transformers":)
        lift $ putStrLn "Done with modification in f"
      g = do
        lift $ putStrLn "Before modification in g"
        modify' (force . ("monad":))
        lift $ putStrLn "Done with modification in g"
  res <- execStateT (concurrentlyS f g) ["!"]
  print res