{-# LANGUAGE UndecidableInstances, DuplicateRecordFields #-}
module Main
where

import Prelude
import Domain
import Optics
import qualified DomainOptics.Deriver as Deriver


main =
  return ()

load (Just (True, False)) Deriver.labelOptic
  "samples/1.yaml"
