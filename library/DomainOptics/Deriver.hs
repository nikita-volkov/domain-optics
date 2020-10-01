module DomainOptics.Deriver
where

import DomainOptics.Prelude
import qualified Domain.Deriver as Domain
import qualified DomainOptics.InstanceDecs as InstanceDecs


{-|
Generates 'LabelOptic' instances for wrappers, enums, products and sums,
automatically choosing the appropriate optic type.

Requires to have the @UndecidableInstances@ extension enabled.
-}
labelOptic =
  Domain.Deriver $ pure . InstanceDecs.labelOptic
