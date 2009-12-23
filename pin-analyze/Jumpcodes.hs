module Jumpcodes where

data Jump =
  Indirect | Direct
  deriving (Eq, Enum, Ord, Read, Show)
