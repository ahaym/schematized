{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Example where

import Schematized

import ExampleSchema

import Control.Monad
import Control.Monad.Reader
import qualified Data.Map as Map
import GHC.TypeLits

getSumZ :: Has XYZSchema s => NotHaxl ()
getSumZ = do
    z <- getFeature @"z"
    s <- getSum
    lift $ print $ s + z

getSum :: Has XYSchema s => NotHaxl Int
getSum = do
    x <- getX
    y <- getY
    return $ x + y

getX :: forall s. (Schema s (), HasFeature s "x") => NotHaxl Int
getX = getFeature @"x"

getY :: forall s. (Schema s (), HasFeature s "y") => NotHaxl Int
getY = getFeature @"y"

main :: IO ()
main = runReaderT (assertSchema @XYZSchema getSumZ) $ Map.fromList [("x", 10), ("y", 1), ("z", 100)]
