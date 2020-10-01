module DomainOptics.InstanceDecs
where

import DomainOptics.Prelude
import Language.Haskell.TH.Syntax
import THLego.Helpers
import qualified Domain.Deriver as Domain
import qualified Data.Text as Text
import qualified Data.Char as Char
import qualified Optics.Core as Optics
import qualified DomainOptics.Util.OpticsTH as OpticsTH
import qualified DomainOptics.Util.DomainTH as DomainTH


-- *
-------------------------

labelOptic (Domain.TypeDec typeName typeDef) =
  case typeDef of
    Domain.ProductTypeDef members ->
      zipWith zipper (enumFrom 0) members
      where
        membersLength =
          length members
        zipper fieldIndex (fieldName, fieldType) =
          OpticsTH.fieldLensLabelOpticInstanceDec
            (textTyLit fieldName)
            (textName typeName)
            (DomainTH.typeType fieldType)
            membersLength
            fieldIndex
    Domain.SumTypeDef structure ->
      fmap mapper structure
      where
        mapper (memberName, subTypes) =
          OpticsTH.prismLabelOpticInstanceDec
            (textTyLit memberName)
            (textName typeName)
            (DomainTH.sumConstructorName typeName memberName)
            (fmap DomainTH.typeType subTypes)
    Domain.EnumTypeDef memberNames ->
      fmap mapper memberNames
      where
        mapper memberName =
          OpticsTH.prismLabelOpticInstanceDec
            (textTyLit memberName)
            (textName typeName)
            (DomainTH.sumConstructorName typeName memberName)
            []
    Domain.WrapperTypeDef type_ ->
      pure $
      OpticsTH.fieldLensLabelOpticInstanceDec
        (textTyLit "value")
        (textName typeName)
        (DomainTH.typeType type_)
        1
        0
    _ ->
      []
