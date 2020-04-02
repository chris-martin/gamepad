{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures -fno-warn-unused-imports #-}

{-# LANGUAGE BlockArguments, ConstraintKinds, DeriveAnyClass, DeriveFoldable, DeriveGeneric, DerivingStrategies, DerivingVia, FlexibleContexts, GeneralizedNewtypeDeriving, LambdaCase, NoImplicitPrelude, NumericUnderscores, PartialTypeSignatures, ScopedTypeVariables, StandaloneDeriving, TypeApplications, TypeFamilies, UndecidableInstances, ViewPatterns #-}

module Gamepad where

import Relude

import qualified Control.Concurrent as Conc
import qualified GHC.Generics as Gen
import qualified Data.Fixed as Num
import qualified Data.Hashable as Hash
import qualified Data.Hashable.Generic as Hash
import qualified Data.Hashable.Lifted as Hash
import qualified Data.HashSet as HashSet
import qualified Data.Vector as V
import qualified System.USB as USB


---  Mains  ---

main :: IO () = runUSB $
    awaitNewDevice >>= liftIO . print

main_printDeviceList :: IO () = runUSB $
    getDDs >>= liftIO . traverse_ print


---  Aliases for commonly used types  ---

type Dev = USB.Device
type DD = USB.DeviceDesc
type Vec = V.Vector


---  USB action type  ---

newtype USB m a = USB (USB.Ctx -> m a)
  deriving (Functor, Applicative, Monad, MonadIO) via ReaderT USB.Ctx m

runUSB :: MonadIO m => USB m a -> m a
runUSB (USB f) = liftIO USB.newCtx >>= f


---  Device discovery  ---

-- Waits for a new device to appear, returns its description.
awaitNewDevice :: USB IO DD = getDDs >>= fix \r dds ->
  do
    delaySeconds 1
    dds' <- getDDs
    maybe (r dds') return (viaNonEmpty head (genDiff dds' dds))

-- Gets a list of all the USB devices currently plugged in.
getDDs :: MonadIO m => USB m (Vec DD)
getDDs = getDevs >>= traverse getDD

-- Gets a list of all the USB devices currently plugged in.
getDevs :: MonadIO m => USB m (Vec Dev)
getDevs = USB (liftIO . USB.getDevices)

getDD :: MonadIO m => Dev -> m DD
getDD = liftIO . USB.getDeviceDesc


---  Doing nothing  ---

delaySeconds :: MonadIO m => Num.Micro -> m ()
delaySeconds (Num.MkFixed x) = liftIO (Conc.threadDelay (fromInteger x :: Int))


---  Diffing two collections where the element type only has a Generic instance  ---

genDiff :: forall a list. (GenHashable a, Foldable list) => list a -> list a -> [a]
genDiff x y = toList (toSet x `genHashSetDifference` toSet y) where toSet :: list a -> GenHashSet a = foldMap (one @(GenHashSet a))

type GenHashable a = (Eq a, Generic a, Hash.GHashable Hash.Zero (Gen.Rep a))

newtype GenHash a = GenHash a
deriving anyclass instance Hash.Hashable1 GenHash
deriving stock instance Eq a => Eq (GenHash a)
deriving stock instance Foldable GenHash
deriving stock instance Generic a => Generic (GenHash a)
deriving stock instance Gen.Generic1 GenHash
instance GenHashable a => Hashable (GenHash a) where hashWithSalt = genHashWithSalt

newtype GenHashSet a = GenHashSet (HashSet (GenHash a))
deriving stock instance Foldable GenHashSet
deriving newtype instance GenHashable a => Semigroup (GenHashSet a)
deriving newtype instance GenHashable a => Monoid (GenHashSet a)
instance GenHashable a => One (GenHashSet a) where type OneItem (GenHashSet a) = a; one = genHashSetOne

genHashSetOne :: forall a. GenHashable a => a -> GenHashSet a
genHashSetOne = GenHashSet . one @(HashSet (GenHash a)) . GenHash

genHashSetDifference :: GenHashable a => GenHashSet a -> GenHashSet a -> GenHashSet a
genHashSetDifference (GenHashSet x) (GenHashSet y) = GenHashSet (HashSet.difference x y)

genHashWithSalt :: GenHashable a => Int -> GenHash a -> Int
genHashWithSalt i (GenHash x) = Hash.genericHashWithSalt i x
