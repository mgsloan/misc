#!/usr/bin/env stack
-- stack script --resolver lts-9.14 --package async --package transformers --package deepseq

-- Switching to the strict state monad, and deeply forcing can cause this to
-- break:
--
-- > Before modification in g
-- > Before modification in f
-- > Done with modification in f
-- > statet-concurrently-1.hs: thread blocked indefinitely in an MVar operation
--
-- This is resolved in statet-concurrently-2.hs

{-# LANGUAGE RecursiveDo #-}

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Control.DeepSeq

concurrentlyS :: StateT s IO a -> StateT s IO b -> StateT s IO (a, b)
concurrentlyS (StateT f) (StateT g) = StateT $ \s0 -> mdo
  ((a, s1), (b, s2)) <- concurrently (f s0) (g s1)
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