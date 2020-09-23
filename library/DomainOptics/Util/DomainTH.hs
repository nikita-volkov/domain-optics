{-|
TH utils for domain.
-}
module DomainOptics.Util.DomainTH
where

import DomainOptics.Prelude
import Language.Haskell.TH
import DomainOptics.Util.TH
import qualified Domain.Deriver as Domain
import qualified Data.Char as Char
import qualified Data.Text as Text


typeType =
  \ case
    Domain.AppType a b ->
      AppT (typeType a) (typeType b)
    Domain.RefType a ->
      typeRefType a
    Domain.ListType ->
      ListT
    Domain.TupleType a ->
      TupleT a

typeRefType =
  ConT . textName . typeRefNameText

typeRefNameText =
  \ case
    Domain.LocalTypeRef a -> a
    Domain.GlobalTypeRef a b -> Text.intercalate "." a <> "." <> b

recordFieldName a b =
  textName (mapFirstChar Char.toLower a <> mapFirstChar Char.toUpper b)

sumConstructorName a b =
  textName (mapFirstChar Char.toUpper b <> a)

mapFirstChar fn =
  foldMap (\ (a, b) -> Text.cons (fn a) b) .
  Text.uncons