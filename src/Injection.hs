{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

module Injection where

import Data.Holmes (Merge)
import Data.JoinSemilattice.Defined (Defined (..))
import Data.JoinSemilattice.Intersect (Intersect, Intersectable)
import qualified Data.JoinSemilattice.Intersect as Intersect
import Data.Kind (Constraint, Type)

class (forall x. c x => Merge (f x))
    => Injection (f :: Type -> Type) (c :: Type -> Constraint) | f -> c where
  injectR :: (c x, c y) => (Maybe (x -> y), Maybe (y -> Maybe x)) -> ((f x, f y) -> (f x, f y))

instance Injection Defined Eq where
  injectR ( fs, gs ) ( xs, ys )
    = ( case ys of
          Unknown   -> Unknown
          Conflict  -> Conflict
          Exactly y -> case gs of 
            Just g  -> 
              case g y of 
                Just x -> Exactly x
                Nothing -> Conflict
            Nothing -> Unknown

      , case xs of
          Unknown   -> Unknown
          Conflict  -> Conflict
          Exactly x -> case fs of Just f  -> Exactly (f x)
                                  Nothing -> Unknown
      )

instance Injection Intersect Intersectable where
  injectR ( fs, gs ) ( xs, ys )
    = ( case gs of 
          Just g  -> 
            case traverse g (Intersect.toList ys) of 
              Just x -> Intersect.fromList x
              Nothing -> Intersect.fromList []
          Nothing -> mempty

      , case fs of Just f  -> Intersect.map f xs
                   Nothing -> mempty
      )
