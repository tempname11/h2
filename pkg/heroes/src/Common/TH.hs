{-# LANGUAGE TemplateHaskell #-}
module Common.TH where

import Extra.Field.Optics.TH
import Extra.Field.Optics.Internal
import Data.List.Split (splitOn)
import Data.Traversable (for)
import Data.Char (toLower)
import Language.Haskell.TH
import Prelude
import Data.Monoid ((<>))

{-
data Test = {
  tp'hex :: Hex
}

makeShorthands ''Test

          |
          v

makeFieldOptics ''Test

type instance Constraint_hex Test = Functor
type instance Field_hex Test = Hex
instance Has_hex Test where
  hex_ = tp'hex_
-}

lensy :: (a, b, c) -> (a, b, c, Type)
lensy (x, y, z) = (x, y, z, ConT ''Functor)

binco :: (a, b, c) -> (a, b, c, Type)
binco (x, y, z) = (x, y, z, ConT ''FunctorMay)

makeShorthands :: Name -> Q [Dec]
makeShorthands tname = do
  let failNamed = fail . (<> nameBase tname) . (<> " for ")
      failNamed :: String -> Q a
      prefix = map toLower $ nameBase tname
  info <- reify tname
  vars <- case info of
    TyConI (dec) ->
      case dec of
        DataD _cxt _tn _tvs _mk cs _derivs -> do
          let y = if (length cs > 1) then binco else lensy
              varsOf con = case con of
                RecC _name xs -> return xs
                NormalC _ _ -> return []
                _ -> failNamed "Expected a record-style constructor"
          vs <- mapM varsOf cs
          return $ fmap y . concat $ vs

        _ -> failNamed "Expected a `data`"
    _ -> failNamed "Expected a type constructor"
  let s = ConT tname

  lensDecs <- makeFieldOptics tname

  ourDecs <- fmap concat $
              for vars $
                \(name,_,a,c) -> do
                  let base = afterSingleQuote (nameBase name)
                  instancesOf prefix base s a c

  return $ lensDecs <> ourDecs

cname :: String -> Name
cname base = mkName $ "Constraint_" <> base

fname :: String -> Name
fname base = mkName $ "Field_" <> base

classname :: String -> Name
classname base = mkName $ "Has_" <> base

-- s: state
-- a: part of state
-- c: constraint (i.e. Functor)
instancesOf :: String -> String -> Type -> Type -> Type -> Q [Dec]
instancesOf prefix base s a c =
  return $ [ins1, ins2, ins3]
    where
    ins1 = TySynInstD (cname base) $ TySynEqn [s] c
    ins2 = TySynInstD (fname base) $ TySynEqn [s] a
    ins3 = InstanceD Nothing [] instanceType [dec]
      where
      dec = ValD (VarP lensName) (NormalB (VarE fname')) []
      lensName = mkName $ base <> "_"
      fname' = mkName $ prefix <> "'" <> base <> "_"
      instanceType = AppT (ConT $ classname base) s

afterSingleQuote :: String -> String
afterSingleQuote s =
  case splitOn "'" s of
    [_, b] -> b
    _ -> s

{-
declareFieldClass "hex"

          |
          v

data family ConstraintOf_hex a :: Constraint
data family FieldOf_hex a
class Has_hex a where
  hex_ :: forall f. (ConstraintOf_hex a) f => (a -> f a) ->
                    (FieldOf_hex a) -> f (Has_hex_F a)
-}

declareShorthand :: String -> Q [Dec]
declareShorthand base = return [dec1, dec2, dec3]
  where
  ar t1 t2 = (ArrowT `ap` t1) `ap` t2
  ap = AppT
  a' = mkName "a"
  dec1 = OpenTypeFamilyD (TypeFamilyHead (cname base) [PlainTV a'] (KindSig kind) Nothing)
    where
    kind = (StarT `ar` StarT) `ar` ConstraintT
  dec2 = OpenTypeFamilyD (TypeFamilyHead (fname base) [PlainTV a'] NoSig Nothing)
    where
  dec3 = ClassD [] (classname base) [PlainTV a'] [] [dec]
    where
    lensName = mkName $ base <> "_"
    a = VarT a'
    f = VarT $ f'
    f' = mkName "f"
    dec = SigD lensName $ ForallT [PlainTV f'] [constraint] type_
    classC = ConT $ cname base
    classF = ConT $ fname base
    constraint = (classC `ap` a) `ap` f
    fun t = (t `ar` (f `ap` t))
    type_ = fun (classF `ap` a) `ar` fun a
