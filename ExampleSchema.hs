{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module ExampleSchema where

import Schematized

import GHC.TypeLits

type XYSchema = '["x", "y"]

type instance Has XYSchema s =
    ( Schema s ()
    , HasFeature s "x"
    , HasFeature s "y"
    )

type XYZSchema = '["x", "z", "y"]

type instance Has XYZSchema s =
    ( Schema s ()
    , Has XYSchema s
    , HasFeature s "z"
    )
