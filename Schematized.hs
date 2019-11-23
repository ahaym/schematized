{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Schematized where

import Control.Monad
import Control.Monad.Reader
import qualified Data.Map as Map
import Data.Proxy
import GHC.Exts
import GHC.TypeLits
import Unsafe.Coerce

type NotHaxl = ReaderT (Map.Map String Int) IO

class Schema (s :: [Symbol]) o | o -> s

newtype Reflector s a = Reflector { runReflector :: Schema s () => NotHaxl a }

class ManySymbolVal (xs :: [Symbol]) where
  manySymbolVal :: proxy xs -> [String]

instance ManySymbolVal '[] where
  manySymbolVal _ = []

instance (KnownSymbol a, ManySymbolVal as) => ManySymbolVal (a ': as) where
  manySymbolVal _ = 
    symbolVal (Proxy :: Proxy a) : manySymbolVal (Proxy :: Proxy as)

assertSchema :: forall (s :: [Symbol])  a. ManySymbolVal s => (Schema s () => NotHaxl a) -> NotHaxl a
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

class HasFeature (s :: [Symbol]) (k :: Symbol)

instance HasFeature (k ': s) k

instance {-# OVERLAPPABLE #-} HasFeature s k => HasFeature (k0 ': s) k

getFeature :: forall x s. (Schema s (), HasFeature s x, KnownSymbol x) => NotHaxl Int
getFeature = do
    env <- ask
    return $ env Map.! (symbolVal (Proxy :: Proxy x))

type family Has (s0 :: [Symbol]) (s1 :: [Symbol]) :: Constraint
