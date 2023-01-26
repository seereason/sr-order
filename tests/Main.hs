{-# LANGUAGE CPP, FlexibleInstances, ScopedTypeVariables, TemplateHaskell, TupleSections, TypeFamilies #-}
{-# OPTIONS -Wmissing-signatures #-}

import Control.Lens
-- import Control.Monad.Extra (ifM)
import Control.Monad.State
import Data.Order as Order
import System.Exit
-- import System.IO
-- import System.Time.Extra
import Test.QuickCheck
import Data.Map as Map
import Text.Printf

main :: IO ()
main = do
  (secs, r) <- Order.tests
  mapM_ (putStrLn . printf "%0.3f") secs
  if isSuccess r then exitSuccess else exitFailure

-- type T = (Int, Map Char Int, Char)

traversalTests :: IO ()
traversalTests = do
  go (Map.fromList [('a', 1 :: Int), ('b', 2), ('c', 3)])
  go (Order.fromPairs [('a', 1), ('b', 2), ('c', 3)] :: Order Char Int)

go :: (TraversableWithIndex a t, Show a, Show b) => t b -> IO ()
go t = do
  evalStateT (itraverseOf_ itraversed f t) t
  where
    f c i = lift $ putStrLn $ show (c, i)
