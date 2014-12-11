{-| Stability: experimental -}
module Data.SciRatio
    (
      -- * The 'SciRatio' type
      SciRatio
    , SciRational
    , (.^)
    , fracSignificand
    , base10Exponent

      -- * Specialized functions
    , (^!)
    , (^^!)
    , fromSciRatio

      -- * Miscellaneous utilities
    , factorizeBase
    , ilogBase

      -- * Deprecated
    , intLog
    ) where
import Data.Ratio ((%), denominator, numerator)
import Data.Hashable (Hashable(hashWithSalt))
infixr 8 ^!, ^^!
infixl 7 :^, .^
infixl 1 ~~

-- Implementation note: a SciRatio must *always* remain in canonical form, so
-- don't use :^ directly unless you're absolutely sure it will stay canonical
-- (use .^ instead if you're unsure).

-- | Represents a fractional number stored in scientific notation: a product
--   of a fractional significand and an integral power of 10.
--
--   - The significand has type @a@ and should be both @'Fractional'@ and
--     @'Real'@.  Although this could be a floating-point type, it is not
--     recommended as floating-points use inexact arithmetic and strange bugs
--     will occur as a result.
--
--   - The exponent has type @b@ and should be @'Integral'@.
--
--   @'SciRatio'@ behaves in the same way as an ordinary @'Data.Ratio.Ratio'@
--   and supports the same operations.  The main property is that it is moxre
--   efficient than @'Data.Ratio.Ratio'@ when the exponent is large:
--
--   >>> read "(5 % 1) .^ 99999999" :: SciRational   -- works fine
--   >>> 5e99999999                 :: Rational      -- takes forever
--
--   Specialized functions are provided in cases where they can be implemented
--   more efficiently than the default implementation.
--
--   The number is always stored in a unique, canonical form: the significand
--   shall never contain factors of 2 and 5 simultaneously, and their
--   multiplicities shall always be nonnegative.  (Note that here we treat the
--   significand as a rational number factorized into a product of prime
--   numbers with /integral/ exponents.)
--
--   __Note__: If inputs differ greatly in magnitude, @('+')@ and @('-')@ can
--             be quite slow: complexity is linear with the absolute
--             difference of the exponents.  Furthermore, the complexity of
--             @'toRational'@ is linear with the magnitude of the exponent.
--             These also apply to any functions that indirectly use these
--             operations (including all operations in @'RealFrac'@ and
--             @'Enum'@).
--
data SciRatio a b = !a :^ !b deriving Eq

-- | A specialization of 'SciRatio'.
type SciRational = SciRatio Rational Integer

instance (Fractional a, Real a, Integral b, Read a, Read b) =>
         Read (SciRatio a b) where
  {-# SPECIALIZE instance Read SciRational #-}
  readsPrec p = readParen (p > prec) $ \ r -> do
    (x,    s) <- readsPrec (succ prec) r
    (".^", t) <- lex s
    (y,    u) <- readsPrec (succ prec) t
    return (x .^ y, u)

instance (Show a, Show b) => Show (SciRatio a b) where
  {-# SPECIALIZE instance Show SciRational #-}
  showsPrec p (x :^ a) = showParen (p > prec) $
                         showsPrec (succ prec) x .
                         showString " .^ " .
                         showsPrec (succ prec) a

instance (Real a, Integral b, Ord a) => Ord (SciRatio a b) where
  {-# SPECIALIZE instance Ord SciRational #-}
  compare (x :^ a) (y :^ b) = case compare sgnX sgnY of
    EQ -> case sgnX of
      EQ -> EQ
      LT -> invert order
      GT -> order
    k  -> k
    where x'        = toRational x
          y'        = toRational y
          sgnX      = compare x 0
          sgnY      = compare y 0
          absM      = abs (numerator x' * denominator y')
          absN      = abs (numerator y' * denominator x')
          invert GT = LT
          invert EQ = EQ
          invert LT = GT
          order     = case (_ilogBase 10 absM, _ilogBase 10 absN) of
            -- initial comparison via floored logarithms to handle easy cases
            -- where exponents are wildly different
            (logM, logN) -> case compare (a + logM) (b + logN) of
              -- compare directly if the exponents are too close
              EQ | a >= b    -> compare (absM * 10 ^ (a - b)) absN
                 | otherwise -> compare absM (absN * 10 ^ (b - a))
              k              -> k

instance (Hashable a, Hashable b) => Hashable (SciRatio a b) where
  {-# SPECIALIZE instance Hashable SciRational #-}
  hashWithSalt s (r :^ e) = hashWithSalt s (magic, r, e)
    where magic = 0xaaa80d6b :: Int

instance (Fractional a, Real a, Integral b) => Num (SciRatio a b) where
  {-# SPECIALIZE instance Num SciRational #-}
  p + q             =  (x + y) .^ c where (x, y, c) = p ~~ q
  p - q             =  (x - y) .^ c where (x, y, c) = p ~~ q
  x :^ a * (y :^ b) =    x * y .^ (a + b)
  abs      (y :^ b) =    abs y :^ b
  negate   (y :^ b) = negate y :^ b
  signum            = (:^ 0) . signum . fracSignificand
  fromInteger       = (.^ 0) . fromInteger

instance (Fractional a, Real a, Integral b) => Fractional (SciRatio a b) where
  {-# SPECIALIZE instance Fractional SciRational #-}
  x :^ a / (y :^ b) =   x / y .^ (a - b)
  recip    (y :^ b) = recip y .^ (-b)
  fromRational      = (.^ 0) . fromRational

instance (Fractional a, Real a, Integral b) => Real (SciRatio a b) where
  {-# SPECIALIZE instance Real SciRational #-}
  toRational (y :^ b) = toRational y * 10 ^^ b

instance (Fractional a, Real a, Integral b) => RealFrac (SciRatio a b) where
  {-# SPECIALIZE instance RealFrac SciRational #-}
  properFraction q = (\ (n, p) -> (n, fromRational p))
                     . properFraction $ toRational q

instance (Fractional a, Real a, Integral b) => Enum (SciRatio a b) where
  {-# SPECIALIZE instance Enum SciRational #-}
  succ               = fromRational . succ . toRational
  pred               = fromRational . pred . toRational
  toEnum             = fromRational . toEnum
  fromEnum           = fromEnum . toRational
  enumFrom           = fmap fromRational . enumFrom . toRational
  enumFromThen   x   = fmap fromRational
                       . enumFromThen (toRational x)
                       . toRational
  enumFromTo     x   = fmap fromRational
                       . enumFromTo (toRational x)
                       . toRational
  enumFromThenTo x y = fmap fromRational
                       . enumFromThenTo (toRational x) (toRational y)
                       . toRational

-- | Precedence of the @('.^')@ operator.
prec :: Int
prec = 7

-- | Construct a number such that
--   @significand .^ exponent == significand * 10 ^^ exponent@.
{-# SPECIALISE (.^) :: Rational -> Integer -> SciRational #-}
(.^) :: (Fractional a, Real a, Integral b) =>
        a                               -- ^ significand
     -> b                               -- ^ exponent
     -> SciRatio a b
x .^ y = canonicalize $ x :^ y

-- | Extract the fractional significand.
fracSignificand :: SciRatio a b -> a
fracSignificand (x :^ _) = x

-- | Extract the base-10 exponent.
base10Exponent :: SciRatio a b -> b
base10Exponent (_ :^ x) = x

-- | Convert into a 'Fractional' number.
--
--   This is similar to 'realToFrac' but much more efficient for large
--   exponents.
{-# RULES "realToFrac/fromSciRational"
            forall (x :: SciRational) . realToFrac x = fromSciRatio x;
          "realToFrac/SciRational"
            realToFrac = id :: SciRational -> SciRational;
          "fromSciRatio/SciRational"
            fromSciRatio = id #-}
{-# NOINLINE [1] fromSciRatio #-}
fromSciRatio :: (Real a, Integral b, Fractional c) => SciRatio a b -> c
fromSciRatio (x :^ y) = realToFrac x * 10 ^^ y

-- | Specialized, more efficient version of @('^^')@.
{-# RULES "(^^)/SciRational" forall (x :: SciRational) . (^^) x = (^^!) x #-}
(^^!) :: (Fractional a, Real a, Integral b, Integral c) =>
         SciRatio a b -> c -> SciRatio a b
(x :^ a) ^^! b = x ^^ b :^ (a * fromIntegral b)

-- | Specialized, more efficient version of @('^')@.
{-# RULES "(^)/SciRational" forall (x :: SciRational) . (^) x = (^!) x #-}
(^!) :: (Real a, Integral b, Integral c) =>
        SciRatio a b -> c -> SciRatio a b
(x :^ a) ^! b = x ^ b :^ (a * fromIntegral b)

-- | Matches the exponents.
(~~) :: (Fractional a, Integral b) => SciRatio a b -> SciRatio a b -> (a, a, b)
x :^ a ~~ y :^ b = (x * 10 ^^ (a - c), y * 10 ^^ (b - c), c)
  where c = if abs a <= abs b then a else b

-- | Factorize a nonzero integer into a significand and a power of the base
--   such that the exponent is maximized:
--
--   > inputInteger = significand * base ^ exponent
--
--   That is, the significand shall not divisible by the base.  The base must
--   be greater than one.
factorizeBase :: (Integral a, Integral b) =>
                 a                      -- ^ base
              -> a                      -- ^ input integer
              -> (a, b)                 -- ^ significand and exponent
factorizeBase _ 0             = error ("Data.SciRatio.factorizeBase: " ++
                                       "input integer must be nonzero")
factorizeBase b n | b  <  2   = error ("Data.SciRatio.factorizeBase: " ++
                                       "base must be greater than one")
                  | otherwise = _factorizeBase b n

-- | Same as @'factorizeBase'@ but doesn't validate the arguments, so it might
--   loop forever or return something nonsensical.
_factorizeBase :: (Integral a, Integral b) => a -> a -> (a, b)
_factorizeBase b n | r  /= 0   = (n,    0)
                   | r' /= 0   = (n'',  e2 + 1)
                   | otherwise = (n''', e2 + 2)
  where (n',   r)  = n   `quotRem` b
        (n'',  e)  = _factorizeBase (b * b) n'
        (n''', r') = n'' `quotRem` b
        e2         = e * 2

-- | Alias of @'factorizeBase'@.
--
--   Note: Despite what the name suggests, the function does /not/ compute the
--         floored logarithm.
{-# DEPRECATED intLog "use @'factorizeBase'@ instead." #-}
intLog :: (Integral a, Integral b) => a -> a -> (a, b)
intLog = factorizeBase

-- | Calculate the floored logarithm of a positive integer.  The base must be
--   greater than one.
ilogBase :: (Integral a, Integral b) =>
            a                           -- ^ base
         -> a                           -- ^ input integer
         -> b                           -- ^ floor of the logarithm
ilogBase b | b < 2     = error ("Data.SciRatio.ilogBase: " ++
                                "base must be greater than one")
           | otherwise = _ilogBase b

-- | Same as @'ilogBase'@ but doesn't validate the arguments, so it might loop
--   forever or return something nonsensical.
_ilogBase :: (Integral a, Integral b) => a -> a -> b
_ilogBase = (fst .) . ilogr
  -- use the trick from section 14.4 of The Haskell 98 Language Report
  where ilogr b n | n  < b    =      (0, n)
                  | n' < b    =     (l2, n')
                  | otherwise = (1 + l2, n' `div` b)
          where (l, n') = ilogr (b * b) n
                l2      = l * 2

-- | Convert into canonical form by removing all factors of 2 and 5 from the
--   denominator and factoring out as many powers of the base as possible.
canonicalize :: (Fractional a, Real a, Integral b) =>
                SciRatio a b -> SciRatio a b
canonicalize (0 :^ _) = 0 :^ 0
canonicalize (r :^ e) =
  let r' = toRational r in
  case (_factorizeBase 10 (numerator r'), _factorizeBase 2 (denominator r')) of
    ((n, ne), (d, e2)) -> case _factorizeBase 5 d of
      (d', e5) -> case compare e2 e5 of
        EQ -> fromRational (n % d') :^ (e + ne - e5)
        LT -> fromRational ((n * 2 ^ (e5 - e2)) % d') :^ (e + ne - e5)
        GT -> fromRational ((n * 5 ^ (e2 - e5)) % d') :^ (e + ne - e2)
