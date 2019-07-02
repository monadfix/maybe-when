{-# LANGUAGE GADTs, DataKinds, TypeOperators, LambdaCase #-}
{-# LANGUAGE StandaloneDeriving, DerivingStrategies #-}

module Data.MaybeWhen
  ( MaybeWhen(WJust, WNothing),
    fromWJust
  ) where

import Control.Applicative

data MaybeWhen p a where
  WJust :: a -> MaybeWhen p a
  WNothing :: MaybeWhen 'True a

deriving instance Eq a => Eq (MaybeWhen p a)
deriving instance Ord a => Ord (MaybeWhen p a)
deriving instance Show a => Show (MaybeWhen p a)
deriving instance (Read a, p ~ 'True) => Read (MaybeWhen p a)

fromWJust :: MaybeWhen 'False a -> a
fromWJust (WJust a) = a

instance Functor (MaybeWhen p) where
  fmap f = \case
    WNothing -> WNothing
    WJust a -> WJust (f a)

instance Applicative (MaybeWhen p) where
  pure = WJust
  (<*>) = \case
    WNothing -> \_ -> WNothing
    WJust f -> \case
      WNothing -> WNothing
      WJust a -> WJust (f a)

instance Monad (MaybeWhen p) where
  (>>=) = \case
    WNothing -> \_ -> WNothing
    WJust a -> \f -> f a

instance Semigroup a => Semigroup (MaybeWhen p a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (MaybeWhen p a) where
  mempty = pure mempty

instance Num a => Num (MaybeWhen p a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  negate = fmap negate
  fromInteger = pure . fromInteger
