module DomainOptics.Deriver
(
  labelOptic,
)
where

import DomainOptics.Prelude
import Language.Haskell.TH.Syntax
import qualified Domain.Deriver as Domain
import qualified Data.Text as Text
import qualified Data.Char as Char
import qualified Optics.Core as Optics


{-|
Generates 'LabelOptic' instances for wrappers, enums, products and sums,
automatically choosing the appropriate optic type.
-}
labelOptic =
  Domain.Deriver $ pure . labelOpticInstanceDecs


-- *
-------------------------

labelOpticInstanceDecs =
  \ case
    Domain.TypeDec a b -> case b of
      Domain.CompositeTypeDef Domain.SumComposition c ->
        fmap (uncurry (sumConLabelOpticInstanceDecs a)) c
      Domain.EnumTypeDef c ->
        fmap (emptyConLensLabelOpticInstanceDecs a) c
      Domain.CompositeTypeDef Domain.ProductComposition c ->
        fmap (uncurry (typeDefFieldLensLabelOpticInstanceDecs a)) c
      Domain.WrapperTypeDef c ->
        [typeDefFieldLensLabelOpticInstanceDecs a "value" c]
      _ ->
        []

emptyConLensLabelOpticInstanceDecs a b =
  emptyConLensLabelOpticInstanceDec
    (textName a)
    (memberTyLit b)
    (sumConstructorName a b)

sumConLabelOpticInstanceDecs a b c =
  case c of
    Domain.TupleType 0 ->
      emptyConLensLabelOpticInstanceDec
        (textName a)
        (memberTyLit b)
        (sumConstructorName a b)
    _ ->
      prismLabelOpticInstanceDec
        (textName a)
        (memberTyLit b)
        (sumConstructorName a b)
        (typeType c)

typeDefFieldLensLabelOpticInstanceDecs a b c =
  fieldLensLabelOpticInstanceDec
    (textName a)
    (memberTyLit b)
    (recordFieldName a b)
    (typeType c)

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


-- *
-------------------------

textName =
  mkName . Text.unpack

recordFieldName a b =
  textName (onTextFirstChar Char.toLower a <> onTextFirstChar Char.toUpper b)

sumConstructorName a b =
  textName (onTextFirstChar Char.toUpper b <> a)

memberTyLit =
  StrTyLit . Text.unpack

onTextFirstChar fn =
  foldMap (\ (a, b) -> Text.cons (fn a) b) .
  Text.uncons


-- * Optic-only TH
-------------------------

{-|
>instance (k ~ A_Prism, a ~ String, b ~ String) => LabelOptic "dog" k Pet Pet a b where
>  labelOptic =
>    prism' Dog (\ case
>      Dog a -> Just a
>      _ -> Nothing)
-}
prismLabelOpticInstanceDec typeName lit conName aAndBType =
  InstanceD Nothing [aPred, bPred, cPred] headType [labelOpticDec]
  where
    aPred =
      eqConstraintT aName aAndBType
    bPred =
      eqConstraintT bName aAndBType
    cPred =
      eqConstraintT cName (ConT ''Optics.A_Prism)
    headType =
      foldl' AppT (ConT ''Optics.LabelOptic) [
        LitT lit,
        VarT cName,
        ConT typeName,
        ConT typeName,
        VarT aName,
        VarT bName
        ]
    labelOpticDec =
      FunD 'Optics.labelOptic [Clause [] (NormalB (prismE conName)) []]

{-|
>prism' Dog (\ case
>  Dog a -> Just a
>  _ -> Nothing)
-}
prismE conName =
  AppE (AppE (VarE 'Optics.prism') (ConE conName))
    (LamE [VarP aName] (CaseE (VarE aName) [
      Match (ConP conName [VarP bName]) (NormalB (AppE (ConE 'Just) (VarE bName))) []
      ,
      Match WildP (NormalB (ConE 'Nothing)) []
      ]))

emptyConLensLabelOpticInstanceDec typeName lit conName =
  InstanceD Nothing [aPred, bPred, cPred] headType [labelOpticDec]
  where
    aPred =
      eqConstraintT aName (ConT ''Bool)
    bPred =
      eqConstraintT bName (ConT ''Bool)
    cPred =
      eqConstraintT cName (ConT ''Optics.A_Lens)
    headType =
      foldl' AppT (ConT ''Optics.LabelOptic) [
        LitT lit,
        VarT cName,
        ConT typeName,
        ConT typeName,
        VarT aName,
        VarT bName
        ]
    labelOpticDec =
      FunD 'Optics.labelOptic [Clause [] (NormalB (emptyConLensE conName)) []]

emptyConLensE conName =
  AppE (AppE (VarE 'Optics.lens) getterE) setterE
  where
    getterE =
      LamE [VarP aName] (CaseE (VarE aName) [
        Match (ConP conName []) (NormalB (ConE 'True)) []
        ,
        Match WildP (NormalB (ConE 'False)) []
        ])
    setterE =
      LamE [VarP aName, VarP bName] (CondE (VarE bName) (ConE conName) (VarE aName))

{-|
>instance (k ~ A_Lens, a ~ String, b ~ String) => LabelOptic "name" k Human Human a b where
>  labelOptic = lensVL $ \f s -> (\v -> s { humanName = v }) <$> f (humanName s)
-}
fieldLensLabelOpticInstanceDec typeName lit fieldName aAndBType =
  InstanceD Nothing [aPred, bPred, cPred] headType [labelOpticDec]
  where
    aPred =
      eqConstraintT aName aAndBType
    bPred =
      eqConstraintT bName aAndBType
    cPred =
      eqConstraintT cName (ConT ''Optics.A_Lens)
    headType =
      foldl' AppT (ConT ''Optics.LabelOptic) [
        LitT lit,
        VarT cName,
        ConT typeName,
        ConT typeName,
        VarT aName,
        VarT bName
        ]
    labelOpticDec =
      FunD 'Optics.labelOptic [Clause [] (NormalB (namedFieldLensE fieldName)) []]

namedFieldLensE fieldName =
  AppE (AppE (VarE 'Optics.lens) getterE) setterE
  where
    getterE =
      VarE fieldName
    setterE = 
      namedFieldSetterE fieldName

namedFieldSetterE fieldName =
  LamE [VarP aName, VarP bName] (RecUpdE (VarE aName) [(fieldName, VarE bName)])

aName =
  mkName "a"

bName =
  mkName "b"

cName =
  mkName "c"

eqConstraintT name aAndBType =
  AppT (AppT EqualityT (VarT name)) aAndBType
