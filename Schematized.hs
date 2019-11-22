{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Schema where

import Control.Monad
import Control.Monad.Reader
import qualified Data.Map as Map
import Data.Type.Bool
import Data.Proxy
import GHC.TypeLits
import Unsafe.Coerce

type NotHaxl = ReaderT (Map.Map String Int) IO

class Schema (s :: [Symbol]) where
    refl :: ()
    refl = ()

newtype Reflector s a = Reflector { runReflector :: Schema s => NotHaxl a }

class ManySymbolVal (xs :: [Symbol]) where
  manySymbolVal :: proxy xs -> [String]

instance ManySymbolVal '[] where
  manySymbolVal _ = []

instance (KnownSymbol a, ManySymbolVal as) => ManySymbolVal (a ': as) where
  manySymbolVal _ = 
    symbolVal (Proxy :: Proxy a) : manySymbolVal (Proxy :: Proxy as)

assertSchema :: forall (s :: [Symbol])  a. ManySymbolVal s => (Schema s => NotHaxl a) -> NotHaxl a
assertSchema m = do
    env <- ask
    lift $ assertInput @s env
    unsafeCoerce (Reflector m :: Reflector s a) ()

assertInput :: forall (s :: [Symbol]). ManySymbolVal s => Map.Map String Int -> IO ()
assertInput i = do
    let ks = manySymbolVal (Proxy :: Proxy s)
    unless (and (map (flip Map.member i) ks)) $
        error "verification failed"
    return ()

type family Head (k :: [Symbol]) where
    Head (k ': _) = k
    Head '[] = ""

type family InList (k :: Symbol) (s :: [Symbol]) where
    InList _ '[] = 'False
    InList k (k ': ks) = 'True
    InList k (l ': ks) = InList k ks

type HasFeature (s :: [Symbol]) (k :: Symbol) = (Schema s, InList k s ~ 'True)

getFeature :: forall s x. (HasFeature s x, KnownSymbol x) => NotHaxl Int
getFeature = do
    env <- ask
    return $ env Map.! (symbolVal (Proxy :: Proxy x))

type MySchema = '["x", "y"]

getSum :: Schema MySchema => NotHaxl ()
getSum = do
    x <- getX @MySchema
    y <- getY @MySchema
    lift $ print $ x + y

getX :: forall s. HasFeature s "x" => NotHaxl Int
getX = getFeature @s @"x"

getY :: forall s. HasFeature s "y" => NotHaxl Int
getY = getFeature @s @"y"

main :: IO ()
main = runReaderT (assertSchema @MySchema getSum) $ Map.fromList [("x", 10), ("y", 2)]
