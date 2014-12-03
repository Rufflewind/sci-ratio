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
    , intLog
    ) where
import Data.Ratio ((%), denominator, numerator)
import Data.Hashable (Hashable(hashWithSalt))
import Data.Monoid
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
  compare (x :^ a) (y :^ b) = case compare s t of
    EQ -> case s of
      EQ -> EQ
      LT -> invert thecase
      GT -> thecase
    k  -> k
    where x'        = toRational x
          y'        = toRational y
          s         = compare x 0
          t         = compare y 0
          absM      = abs (numerator x' * denominator y')
          absN      = abs (numerator y' * denominator x')
          invert GT = LT
          invert EQ = EQ
          invert LT = GT
          thecase   = case (intLog 10 absM, intLog 10 absN) of
            -- initial comparison via integer logs to handle easy cases where
            -- exponents are wildly different
            ((lremM, ilogM), (lremN, ilogN)) ->
             let digM = 1 + imLog 10 lremM
                 digN = 1 + imLog 10 lremN
             in
              case compare 0 ( b - a
                             + ilogN - ilogM
                             + fromInteger digN - fromInteger digM ) of
                -- compare directly when the int logs alone aren't enough
                EQ -> case compare digM digN of
                        EQ -> compare lremM lremN
                        LT -> compare lremM (dropLSD (digN - digM) lremN) <> LT
                        GT -> compare (dropLSD (digM - digN) lremM) lremN <> GT
                k  -> k

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

-- | Extract the largest power of the given base that divides the input
--   integer.  Returns the significand and exponent, satisfying:
--
--   > input_integer = significand * base ^ exponent
--
--     * If the input integer is zero, then zeros are returned.
--     * If the input integer is negative, the significand is negative.
--
--   This is similar to computing the floored logarithm and its associated
--   remainder.
intLog :: (Integral a, Integral b) =>
          a                             -- ^ base
       -> a                             -- ^ input integer
       -> (a, b)                        -- ^ significand and exponent
intLog base = go 0 1 0
  where go  _    _   _ 0 = (0, 0)
        go  _    0   e i = (i, e)
        go maxe next e i =
          if i `mod` power == 0
          then go maxe'   next'  (e + next) (i `div` power)
          else go next'' next'' e i
          where power  = base ^ next
                maxe'  = if maxe == 0 then maxe     else maxe - next
                next'  = if maxe == 0 then next * 2 else maxe `div` 2
                next'' = next `div` 2

-- | Integer log base (c.f. Haskell report 14.4):
imLog :: Integral a => a ->a -> a
imLog b n
  = if n < b then
      0
    else
     let
       l = 2 * imLog (b*b) n
     in
       doDiv (n`div`(b^l)) l
  where
    doDiv x l = if x < b then l else doDiv (x`div`b) (l+1)

-- | @dropLSD k n@  drops the least @k@ significant decimal digits
--   from @n@ (by successive division)
dropLSD :: Integer -> Integer -> Integer
dropLSD k n
  | k <= 0    = n
  | otherwise = dropLSD (k-1) (n `div` 10)

-- | Convert into canonical form by removing all factors of 2 and 5 from the
--   denominator and factoring out as many powers of the base as possible.
canonicalize :: (Fractional a, Real a, Integral b) =>
                SciRatio a b -> SciRatio a b
canonicalize (0 :^ _) = 0 :^ 0
canonicalize (r :^ e) =
  let r' = toRational r in
  case (intLog 10 (numerator r'), intLog 2 (denominator r')) of
    ((n, ne), (d, e2)) -> case intLog 5 d of
      (d', e5) -> case compare e2 e5 of
        EQ -> fromRational (n % d') :^ (e + ne - e5)
        LT -> fromRational ((n * 2 ^ (e5 - e2)) % d') :^ (e + ne - e5)
        GT -> fromRational ((n * 5 ^ (e2 - e5)) % d') :^ (e + ne - e2)
