{-# LANGUAGE CPP, FlexibleInstances, ScopedTypeVariables, TemplateHaskell, TupleSections, TypeFamilies #-}

import Control.Monad.Extra (ifM)
import Data.Order as Order
import System.Exit
import Test.QuickCheck

main :: IO ()
main = ifM (isSuccess <$> Order.tests) exitSuccess exitFailure
