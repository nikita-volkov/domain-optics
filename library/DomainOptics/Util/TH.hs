{-|
General TH utils.
-}
module DomainOptics.Util.TH
where

import DomainOptics.Prelude
import Language.Haskell.TH
import qualified Optics.Core as Optics
import qualified Data.Text as Text
import qualified TemplateHaskell.Compat.V0208 as Compat


textName :: Text -> Name
textName =
  mkName . Text.unpack

textTyLit :: Text -> TyLit
textTyLit =
  StrTyLit . Text.unpack

noBang :: Bang
noBang =
  Bang NoSourceUnpackedness NoSourceStrictness

fieldBang :: Bang
fieldBang =
  Bang NoSourceUnpackedness SourceStrict

multiAppT :: Type -> [Type] -> Type
multiAppT base args =
  foldl' AppT base args

multiAppE :: Exp -> [Exp] -> Exp
multiAppE base args =
  foldl' AppE base args

appliedTupleT :: [Type] -> Type
appliedTupleT a =
  foldl' AppT (TupleT (length a)) a

appliedTupleOrSingletonT :: [Type] -> Type
appliedTupleOrSingletonT =
  \ case
    [a] -> a
    a -> appliedTupleT a

indexName :: Int -> Name
indexName =
  mkName . showChar '_' . show

enumNames :: Int -> [Name]
enumNames =
  fmap indexName . enumFromTo 0 . pred

aName =
  mkName "a"

bName =
  mkName "b"

cName =
  mkName "c"

eqConstraintT :: Name -> Type -> Type
eqConstraintT name =
  AppT (AppT EqualityT (VarT name))


-- * Standard lambdas
-------------------------

{-|
Lambda expression, which extracts a product member by index.
-}
productGetterE :: Name -> Int -> Int -> Exp
productGetterE conName numMembers index =
  LamE [pat] exp
  where
    varName =
      indexName index
    pat =
      ConP conName pats
      where
        pats =
          replicate index WildP <>
          pure (VarP varName) <>
          replicate (numMembers - index - 1) WildP
    exp =
      VarE varName

{-|
Lambda expression, which sets a product member by index.
-}
productSetterE :: Name -> Int -> Int -> Exp
productSetterE conName numMembers index =
  LamE [stateP, valP] exp
  where
    valName =
      mkName "x"
    stateP =
      ConP conName pats
      where
        pats =
          fmap (VarP . indexName) (enumFromTo 0 (pred numMembers))
    valP =
      VarP valName
    exp =
      foldl' AppE (ConE conName) (fmap VarE names)
      where
        names =
          fmap indexName (enumFromTo 0 (pred index)) <>
          pure valName <>
          fmap indexName (enumFromTo (succ index) (pred numMembers))

adtConstructorNarrowerE :: Name -> Int -> Exp
adtConstructorNarrowerE conName numMembers =
  LamE [VarP aName] exp
  where
    exp =
      CaseE (VarE aName) [positive, negative]
      where
        positive =
          Match (ConP conName (fmap VarP varNames)) (NormalB exp) []
          where
            varNames =
              fmap indexName (enumFromTo 0 (pred numMembers))
            exp =
              AppE (ConE 'Just) (Compat.tupE (fmap VarE varNames))
        negative =
          Match WildP (NormalB (ConE 'Nothing)) []

singleConstructorAdtToTupleE :: Name -> Int -> Exp
singleConstructorAdtToTupleE conName numMembers =
  LamE [pat] exp
  where
    varNames =
      fmap indexName (enumFromTo 0 (pred numMembers))
    pat =
      ConP conName (fmap VarP varNames)
    exp =
      Compat.tupE (fmap VarE varNames)

tupleToProductE :: Name -> Int -> Exp
tupleToProductE conName numMembers =
  LamE [pat] exp
  where
    varNames =
      fmap indexName (enumFromTo 0 (pred numMembers))
    pat =
      TupP (fmap VarP varNames)
    exp =
      multiAppE (ConE conName) (fmap VarE varNames)

namedFieldSetterE :: Name -> Exp
namedFieldSetterE fieldName =
  LamE [VarP aName, VarP bName] (RecUpdE (VarE aName) [(fieldName, VarE bName)])
