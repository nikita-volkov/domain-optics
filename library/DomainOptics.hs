module DomainOptics
where

import DomainOptics.Prelude
import qualified DomainCore.Deriver as Deriver
import qualified DomainOptics.InstanceDecs as InstanceDecs


{-|
Generates 'LabelOptic' instances for wrappers, enums, products and sums,
automatically choosing the appropriate optic type.

Requires to have the @UndecidableInstances@ extension enabled.
-}
labelOpticDeriver =
  Deriver.effectless InstanceDecs.labelOptic
