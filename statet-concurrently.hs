#!/usr/bin/env stack
-- stack script --resolver lts-9.14 --package async --package transformers

-- While watching this interesting talk by Michael Snoyman:
--
-- "Monad Transformer State: Everything you didn't want to know"
-- https://www.youtube.com/watch?v=KZIN9f9rI34&feature=youtu.be
--
-- It ocurred to me that MonadFix / RecursiveDo could be used to write an
-- implementation of 'concurrently' for State that does no discarding! This is
-- not really very useful, it is just an interesting "why not" exercise.
--
-- Output looks like this:
--
-- > Before modification in g
-- > Done with modification in g
-- > Before modification in f
-- > Done with modification in f
-- > ["monad","transformers","!"]
--
-- Note that g finishes executing before f, even though the state g runs in
-- depends on f. This is because it is lazy with respect to the input state.


{-# LANGUAGE RecursiveDo #-}

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy

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
        modify ("transformers":)
        lift $ putStrLn "Done with modification in f"
      g = do
        lift $ putStrLn "Before modification in g"
        modify (("monad":))
        lift $ putStrLn "Done with modification in g"
  res <- execStateT (concurrentlyS f g) ["!"]
  print res