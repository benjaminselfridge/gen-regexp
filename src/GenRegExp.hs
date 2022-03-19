{-# LANGUAGE MonadComprehensions #-}

-- | Very simple module for generating strings from Kleene-style regular
-- expressions. Uses @control-monad-omega@ to get a breadth-first, rather than
-- depth-first, ordering of the string set. In this way, we can guarantee that
-- if @s@ is a string in the set of strings described by regular expression @r@,
-- then @elem s r@ will return @True@ in finite time.
--
-- Consider the expression @(0|(1(01*0)*1))*@, representing the strings
-- corresponding to all binary numbers that are multiples of three. We can
-- encode this as:
--
-- @
-- r = star (alt [ constant '0'
--               , cat [ constant '1'
--                     , star (cat [ constant '0'
--                           , (star (constant '1'))
--                           , constant '0'])
--                     , constant '1'
--                     ]
--               ])
-- @
--
-- Now, if we @generate r@, we get a infinite list of strings from the language
-- described by @r@:
--
-- >>> take 10 (generate r)
-- ["","0","11","00","1001","011","000","10101","110","0011"]
--
-- Eventually, every string in the language will show up (but it might take a
-- while!)
module GenRegExp
  ( RegExp(..)
  , generate
  -- * Building regular expressions
  , emptySet
  , emptyString
  , constant
  , oneOf
  , alt
  , cat
  , plus
  , star
  ) where

import Control.Monad.Omega

-- | Kleene-style regular expressions.
data RegExp a
  = EmptySet
  | EmptyString
  | Constant a
  | Alt (RegExp a) (RegExp a)
  | Cat (RegExp a) (RegExp a)
  | Plus (RegExp a)
  deriving (Show, Read)

-- | The empty set of strings.
emptySet :: RegExp a
emptySet = EmptySet

-- | The singleton set containing the empty string.
emptyString :: RegExp a
emptyString = EmptyString

-- | The singleton set containing the string of length one corresponding to the
-- given token.
constant :: a -> RegExp a
constant = Constant

-- | The set of all strings of length one from a given token set.
oneOf :: [a] -> RegExp a
oneOf = alt . map Constant

-- | @alt [a, b, c, ..] === (a|b|c|...)@
alt :: [RegExp a] -> RegExp a
alt [] = EmptySet
alt as = foldr1 Alt as

-- | @cat [a, b, c, ..] === abc...@
cat :: [RegExp a] -> RegExp a
cat [] = EmptyString
cat as = foldr1 Cat as

-- | The @+@ operator (one or more concatenated).
plus :: RegExp a -> RegExp a
plus = Plus

-- | The @*@ operator (zero or more concatenated).
star :: RegExp a -> RegExp a
star = Alt EmptyString . Plus

-- | Generate all strings from a given regular expression in a sensible order
-- (breadth-first).
generate :: RegExp a -> [[a]]
generate EmptySet = []
generate EmptyString = [[]]
generate (Constant a) = [[a]]
generate (Alt r s) = diagonal [generate r, generate s]
generate (Cat r s) = runOmega [ x ++ y | x <- each (generate r)
                                       , y <- each (generate s) ]
generate (Plus r) = diagonal (generate . cat <$> tau r)

-- | Given a single a, produce the infinite list @[[a], [a,a], [a,a,a], ...]
tau :: a -> [[a]]
tau a = [a] : [ a : as | as <- tau a ]
