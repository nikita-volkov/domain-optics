{-# LANGUAGE UndecidableInstances #-}
module Main
where

import Prelude
import Domain
import Optics
import qualified DomainOptics.Deriver as Deriver


main =
  return ()

load "samples/1.yaml" Deriver.labelOptic
