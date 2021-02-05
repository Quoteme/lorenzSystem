{-# LANGUAGE DeriveGeneric #-}
module Lib
    where

import GHC.Generics
import Data.Aeson
import Data.List
import Graphics.Matplotlib

newtype State a = State [a] deriving (Show, Eq, Generic)

instance Functor State where
  fmap f (State x) = State (map f x)

instance Applicative State where
  pure x = State [x]
  State f <*> State x = State (zipWith (\f' x' -> f' x') f x)

instance ToJSON a => ToJSON (State a) where
  toEncoding = genericToEncoding defaultOptions

type Derivative a = State a

type System a = State a -> Derivative a

-- Applies a System (of differential equations) to a state of numeric
-- values by adding the derivative
apply :: Num a => a -> System a -> State a -> State a
apply accuracy f x = fmap (\x y -> accuracy*x+y) (f x) <*> x

trajectory :: System Double -> Double -> Int -> [State Double] -> [State Double]
trajectory _ _ _ []     = []
trajectory _ _ 0 x      = x
trajectory f accuracy n (x:xs) =
  trajectory f accuracy (n-1) (apply accuracy f x:x:xs)

point3d :: State Double -> [Double]
point3d (State [x,y,z]) = [x,y,z]

plot3d l = axis3DProjection
  # "\nax.scatter("
  # show x
  # ","
  # show y
  # ","
  # show z
  # ", c='red', marker='.')"
  where
    x:y:z:_ = transpose ( map point3d l)

lorenzSystem :: Double -> Double -> Double -> System Double
lorenzSystem sigma rho beta (State [x,y,z]) =
  State [ sigma*(y-x)
        , x*(rho-z)-y
        , x*y-beta*z]

lorenzSystemWikipedia = lorenzSystem 10 28 (8/3)
