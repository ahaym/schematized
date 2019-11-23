{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Interactive where

import Schematized
import Example

instance Schema '[] ()

instance HasFeature '[] s
