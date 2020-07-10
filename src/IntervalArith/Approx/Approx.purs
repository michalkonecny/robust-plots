{-
  Based on CDAR.Approx fork by Michal Konečný (https://github.com/michalkonecny/cdar)
  originally developed by Jens Blanck (https://github.com/jensblanck/cdar).
-}
module IntervalArith.Approx where

import Prelude
import Control.Biapply (biapply)
import Data.BigInt (abs)
import Data.Enum (fromEnum)
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Data.Ord (signum)
import Data.Ratio (denominator, numerator, (%))
import Data.Tuple (Tuple(..))
import Data.Typelevel.Undefined (undefined)
import Effect.Exception.Unsafe (unsafeThrow)
import FFI.BigInt (bitLength)
import IntervalArith.Dyadic (Dyadic, dyadicToNumber, (:^))
import IntervalArith.Dyadic as Dyadic
import IntervalArith.Extended (Extended(..))
import IntervalArith.Misc (class ToRational, Integer, Rational, big, bit, ceilingRational, divMod, integerLog2, roundRational, scale, shift, testBit, toRational, two, (^))

-- | A type synonym. Used to denote number of bits after binary point.
type Precision
  = Int

{-|
= Centred Dyadic Approximations
There are two constructors for approximations:

- 'Approx' is encodes some finite interval with dyadic endpoints. A real
  number is /approximated/ by the approximation is it belongs to the interval.
- 'Bottom' is the trivial approximation that approximates all real numbers.

The four fields of an @Approx m e s@ should be thought of as:

[@mb@] the midpoint bound, ie maximum bits available for the midpoint
[@m@] the midpoint
[@e@] the error term
[@s@] the exponent

Thus, a value @Approx p m e s@ is to be interpreted as the interval
[(m-e)*2^s, (m+e)*2^s] where |m| <= 2^p.

== Centred intervals
We have opted for a centred representation of the intervals. It is also
possible to represent the endpoints as 'Dyadic' numbers. The rationale for a
centred repersentation is that we often normalise an approximation @Approx p m e
s@ so that @e@ is limited in size. This allows many operations to only work on
one large number @m@.

== Potential for overflow
Since the last field (the exponent) is only an 'Int' it may overflow. This is
an optimisation that was adopted since it seems unlikely that overflow in a 64
bit Int exponent would occur. In a 32 bit system, this is potentially an
issue.

The 'Integer' data type is unbonded, but is, of course, bounded by the
available memory available in the computer. No attempt has been made to check
for exhausted memory.

== Approximations as a Domain

Ordered by reverse inclusion the dyadic intervals encoded by the 'Approx'
approximations (including 'Bottom') constitute the compact elements of a Scott
domain /D/. (It is a substructure of the (algebraic) interval domain.)
We will identify our approximations with the compact elements of /D/.

Increasing sequences in /D/ have suprema. A sequence /converges/ if the length
of the approximations tend to zero. The supremum of a converging sequence is a
singleton set containing a real number. Let ρ be the map taking a converging
sequence to the unique real number in the supremum. The computations on
(computable) real numbers is via this representation map ρ.

There is no check that the sequences we have are in fact increasing, but we
are assuming that all sequences are pairwise consistent. We can thus create an
increasing sequence by considering the sequence of finite suprema. For
correctness, we have to ensure that all operations done on consistent
sequences result in consistent sequences. If non-consistent sequences are
somehow input we can make no guarantees at all about the computed value.

Note, that we cannot ensure that converging sequences are mapped to converging
sequences because of properties of computable real arithmetic. In particular,
at any discuntinuity, it is impossible to compute a converging sequence.
-}
data Approx
  = Approx Int Integer Integer Int
  | Bottom

-- | Syntactic equality
derive instance eqApprox :: Eq Approx

instance showApprox :: Show Approx where
  show (Approx b m e s) = intercalate " " [ "Approx", show b, "(", show m, ") (", show e, ")", show s ]
  show Bottom = "Bottom"

{-|
    Give the "bound on midpoint bit-size" component of an 'Approx'.

    The midpoint coponent should always be bounded by this as follows:
    @ abs m <= 2^mb@.
-}
mBound :: Approx -> Int
mBound (Approx mb _ _ _) = mb

mBound Bottom = unsafeThrow "mBound Bottom"

approxAutoMB :: Integer -> Integer -> Int -> Approx
approxAutoMB m e s = Approx mb m e s
  where
  ame = (abs m) + e

  mb
    | ame <= big 4 = 2
    | otherwise = bitLength ame

enforceMB :: Approx -> Approx
enforceMB Bottom = Bottom

enforceMB a@(Approx mb m e s) =
  let
    result
      | m_size <= mb = a
      | abs m <= (big 1) = a
      | otherwise = Approx mb m' e'' (s + d)

    m_size = bitLength m -- |m| <= 2^m_size

    d = m_size - mb

    m' = shift m (-d) -- we have: m' * 2^d <= m

    e' = (big 1) + (shift (e - (big 1)) (-d)) -- we have: 0 <= e <= e' * 2^d

    e''
      | m == shift m' d = e' -- no loss of information
      | otherwise = (big 1) + e'
  in
    result

approxMB :: Int -> Integer -> Integer -> Int -> Approx
approxMB mb m e s = enforceMB $ Approx mb m e s

approxMB2 :: Int -> Int -> Integer -> Integer -> Int -> Approx
approxMB2 mb1 mb2 m e s = enforceMB $ Approx (mb1 `max` mb2) m e s

mapMB :: (Int -> Int) -> Approx -> Approx
mapMB f (Approx mb m e s) = approxMB (f mb) m e s

mapMB _f Bottom = Bottom

setMB :: Int -> Approx -> Approx
setMB mb = mapMB (const mb)

-- |Construct a centred approximation from the end-points.
endToApprox :: Int -> Extended Dyadic -> Extended Dyadic -> Approx
endToApprox mb (Finite l) (Finite u)
  | u < l = Bottom -- Might be better with a signalling error.
  | otherwise =
    let
      a@(m :^ s) = scale (l + u) (-1)

      (n :^ t) = u - a

      r = min s t

      m' = scale m (s - r)

      n' = scale n (t - r)
    in
      (approxMB mb m' n' r)

endToApprox _ _ _ = Bottom

-- Interval operations
-- |Gives the lower bound of an approximation as an 'Extended' 'Dyadic' number.
lowerBound :: Approx -> Extended Dyadic
lowerBound (Approx _ m e s) = Finite ((m - e) :^ s)

lowerBound Bottom = NegInf

-- |Gives the upper bound of an approximation as an 'Extended' 'Dyadic' number.
upperBound :: Approx -> Extended Dyadic
upperBound (Approx _ m e s) = Finite ((m + e) :^ s)

upperBound Bottom = PosInf

bounds :: Approx -> Tuple (Extended Dyadic) (Extended Dyadic)
bounds (Approx _ m e s) = Tuple (Finite ((m - e) :^ s)) (Finite ((m + e) :^ s))

bounds Bottom = Tuple NegInf PosInf

boundsR :: Approx -> Tuple (Extended Rational) (Extended Rational)
boundsR a = biapply (Tuple f f) (bounds a)
  where
  f = map toRational

boundsA :: Approx -> Tuple Approx Approx
boundsA a = Tuple (lowerA a) (upperA a)

toNumber :: Approx -> Number
toNumber a = (l + u) / two
  where
  (Tuple l u) = boundsNumber a

boundsNumber :: Approx -> Tuple Number Number
boundsNumber a = biapply (Tuple f f) (bounds a)
  where
  f PosInf = 1.0 / 0.0

  f NegInf = -1.0 / 0.0

  f (Finite d) = dyadicToNumber d

-- | Gives the lower bound of an 'Approx' as an exact 'Approx'.
lowerA :: Approx -> Approx
lowerA Bottom = Bottom

lowerA (Approx mb m e s) = Approx mb (m - e) (big 0) s

-- |Gives the upper bound of an 'Approx' as an exact 'Approx'.
upperA :: Approx -> Approx
upperA Bottom = Bottom

upperA (Approx mb m e s) = Approx mb (m + e) (big 0) s

-- |Gives the mid-point of an approximation as a 'Maybe' 'Dyadic' number.
centre :: Approx -> Maybe Dyadic
centre (Approx _ m _ s) = Just (m :^ s)

centre _ = Nothing

-- |Gives the centre of an 'Approx' as an exact 'Approx'.
centreA :: Approx -> Approx
centreA Bottom = Bottom

centreA (Approx mb m _e s) = Approx mb m (big 0) s

-- |Gives the radius of an approximation as a 'Dyadic' number. Currently a
-- partial function. Should be made to return an 'Extended' 'Dyadic'.
radius :: Approx -> Extended Dyadic
radius (Approx _ _ e s) = Finite (e :^ s)

radius _ = PosInf

-- |Gives the lower bound of an approximation as an 'Extended' 'Dyadic' number.
diameter :: Approx -> Extended Dyadic
diameter (Approx _ _ e s) = Finite $ (Dyadic.fromInt 2) * (e :^ s)

diameter _ = PosInf

-- |Computes the precision of an approximation. This is roughly the number of
-- correct bits after the binary point.
precision :: Approx -> Extended Precision
precision (Approx _ _ e _)
  | e == zero = PosInf

precision (Approx _ _ e s) = Finite $ -s - (integerLog2 e) - 1

precision Bottom = NegInf

-- |Computes the significance of an approximation. This is roughly the number
-- of significant bits.
significance :: Approx -> Extended Int
significance (Approx _ _ e _)
  | e == zero = PosInf

significance (Approx _ m _ _)
  | m == zero = NegInf

significance (Approx _ m e _)
  | e == one = Finite $ integerLog2 (abs m) - 1

significance (Approx _ m e _) = Finite $ (integerLog2 (abs m)) - (integerLog2 (e - one)) - 1

significance Bottom = NegInf

-- |Returns 'True' if the approximation is exact, i.e., it's diameter is 0.
exact :: Approx -> Boolean
exact (Approx _ _ e _) = e == (big 0)

exact Bottom = false

-- | Checks if the centre of an approximation is not 0.
nonZeroCentredA :: Approx -> Boolean
nonZeroCentredA Bottom = false

nonZeroCentredA (Approx _ m _ _)
  | m == zero = false

nonZeroCentredA _ = true

-- |Checks if a number is approximated by an approximation, i.e., if it
-- belongs to the interval encoded by the approximation.
approximatedBy :: forall a. ToRational a => a -> Approx -> Boolean
approximatedBy _ Bottom = true

approximatedBy r d =
  let
    r' = toRational r
  in
    toRational (lowerBound d) <= r' && r' <= toRational (upperBound d)

-- |A partial order on approximations. The first approximation is better than
-- the second if it is a sub-interval of the second.
better :: Approx -> Approx -> Boolean
better d e = (lowerBound e <= lowerBound d) && (upperBound d <= upperBound e)

worse :: Approx -> Approx -> Boolean
worse = flip better

consistent :: Approx -> Approx -> Boolean
consistent Bottom _ = true

consistent _ Bottom = true

consistent d e = not $ (upperBound e < lowerBound d) || (upperBound d < lowerBound e)

infix 4 worse as ⊑

infix 4 better as ⊒

-- |Turns a 'Dyadic' number into an exact approximation.
fromDyadic :: Dyadic -> Approx
fromDyadic (m :^ s) = approxAutoMB m (big 0) s

-- |Turns a 'Dyadic' number into an exact approximation.
fromDyadicMB :: Int -> Dyadic -> Approx
fromDyadicMB mb (m :^ s) = approxMB mb m (big 0) s

fromInt :: Int -> Approx
fromInt i = approxAutoMB (big i) (big 0) 0

fromInteger :: Integer -> Approx
fromInteger i = approxAutoMB i (big 0) 0

fromIntegerMB :: Int -> Integer -> Approx
fromIntegerMB mb i = approxMB mb i (big 0) 0

fromRationalPrec :: Precision -> Rational -> Approx
fromRationalPrec t r = approxAutoMB m e (-t - 1)
  where
  p = numerator r

  q = denominator r

  -- r_scaled_rounded = round (r*2^^t)
  p_shifted = shift p (t + 1)

  q_shifted = shift q 1

  r_scaled_rounded = (p_shifted + q) `div` q_shifted

  -- m = 2 * r_scaled_rounded
  m = (shift r_scaled_rounded 1)

  isRounded = p_shifted `mod` q_shifted /= zero

  e = big $ fromEnum isRounded

{-|
  Make an Approx hull of the given two Rational numbers `l` and `u`, possibly overapproximating up to the given precision.

  Precondition: `l <= u`.
-}
fromRationalBoundsPrec :: Precision -> Rational -> Rational -> Approx
fromRationalBoundsPrec t l u = approxAutoMB m (e + roundingCompensation) (-t - 1)
  where
  cR = (l + u) / two

  eR = (u - l) / two

  scaleAndRound r = Tuple rScaled isRounded
    where
    p = numerator r

    q = denominator r

    -- r_scaled_rounded = round (r*2^^t)
    p_shifted = shift p (t + 1)

    q_shifted = shift q 1

    r_scaled_rounded = (p_shifted + q) `div` q_shifted

    -- m = 2 * r_scaled_rounded
    rScaled = (shift r_scaled_rounded 1)

    isRounded = p_shifted `mod` q_shifted /= zero

  Tuple m mIsRounded = scaleAndRound cR

  Tuple e eIsRounded = scaleAndRound eR

  roundingCompensation = big $ fromEnum mIsRounded + fromEnum eIsRounded

instance semiringApprox :: Semiring Approx where
  zero = fromInt 0
  one = fromInt 1
  add (Approx mb1 m e s) (Approx mb2 n f t)
    | s >= t =
      let
        k = s - t
      in
        approxMB2 mb1 mb2 (shift m k + n) (shift e k + f) t
    | s < t =
      let
        k = t - s
      in
        approxMB2 mb1 mb2 (m + shift n k) (e + shift f k) s
  add _ _ = Bottom
  mul (Approx mb1 m e s) (Approx mb2 n f t) = result
    where
    result
      | am >= e && an >= f && a > zero = approxMB2 mb1 mb2 (a + d) (ab + ac) u
      | am >= e && an >= f && a < zero = approxMB2 mb1 mb2 (a - d) (ab + ac) u
      | am < e && n >= f = approxMB2 mb1 mb2 (a + b) (ac + d) u
      | am < e && -n >= f = approxMB2 mb1 mb2 (a - b) (ac + d) u
      | m >= e && an < f = approxMB2 mb1 mb2 (a + c) (ab + d) u
      | -m >= e && an < f = approxMB2 mb1 mb2 (a - c) (ab + d) u
      | a == zero = approxMB2 mb1 mb2 zero (ab + ac + d) u
      | am < e && an < f && a > zero && ab > ac = approxMB2 mb1 mb2 (a + ac) (ab + d) u
      | am < e && an < f && a > zero && ab <= ac = approxMB2 mb1 mb2 (a + ab) (ac + d) u
      | am < e && an < f && a < zero && ab > ac = approxMB2 mb1 mb2 (a - ac) (ab + d) u
      | am < e && an < f && a < zero && ab <= ac = approxMB2 mb1 mb2 (a - ab) (ac + d) u
      | otherwise = undefined

    am = (abs m)

    an = (abs n)

    a = m * n

    b = m * f

    c = n * e

    d = e * f

    ab = (abs b)

    ac = (abs c)

    u = s + t
  mul _ _ = Bottom

instance ringApprox :: Ring Approx where
  sub (Approx mb1 m e s) (Approx mb2 n f t)
    | s >= t =
      let
        k = s - t
      in
        approxMB2 mb1 mb2 (shift m k - n) (shift e k + f) t
    | s < t =
      let
        k = t - s
      in
        approxMB2 mb1 mb2 (m - shift n k) (e + shift f k) s
  sub _ _ = Bottom

instance commutativeRingApprox :: CommutativeRing Approx

-- |Square an approximation. Gives the exact image interval, as opposed to
-- multiplicating a number with itself which will give a slightly larger
-- interval due to the dependency problem.
sqrA :: Approx -> Approx
sqrA Bottom = Bottom
sqrA (Approx mb m e s) = result
  where
  am = abs m
  result
    | am > e = approxMB mb (m^2 + e^2) (two*am*e) (2*s)
    | otherwise = let m' = (am + e)^2 in approxMB mb m' m' (2*s-1)

instance divisionRingApprox :: DivisionRing Approx where
  recip = recipA

instance euclideanRingApprox :: EuclideanRing Approx where
  degree _ = 1
  mod Bottom _ = Bottom
  mod (Approx mb m e s) _ = Approx mb (big 0) (big 0) s -- preserving mb and s
  div Bottom _ = Bottom
  div a b = a * (recipA b)

-- |[Copied from CDAR] Number of bits that error term is allowed to take up. A larger size allows
-- |for more precise but slightly more costly computations. The value here is
-- |uggested by test runs.
errorBits :: Int
errorBits = 10

errorBound :: Integer
errorBound = two ^ errorBits

errorBitsMB :: Int
errorBitsMB = 1

errorBoundMB :: Integer
errorBoundMB = two ^ errorBitsMB

-- |Compute the reciprocal of an approximation. The number of bits after the
-- |binary point is bounded by the 'midpoint bound' if the input is exact.
-- |Otherwise, a good approximation with essentially the same significance as
-- |the input will be computed.
recipA :: Approx -> Approx
recipA Bottom = Bottom

recipA (Approx mb m e s)
  | e == zero && m /= zero =
    let
      s' = integerLog2 (abs m)
    in
      if abs m == bit s' then
        Approx mb (signum m) zero (-s - s')
      else
        Approx mb
          (roundRational (bit (mb + s') % m))
          one
          (-mb - s - s')
  | (abs m) > e =
    let
      d = m * m - e * e

      d2 = shift d (-1)

      s' = integerLog2 d + 2 * errorBits
    in
      boundErrorTerm
        $ approxMB mb
            ((shift m s' + d2) `div` d)
            ((shift e s' + d2 + one) `div` d + one)
            (-s - s')
  --  (abs m) > e = let d = m*m-e*e
  --                     s' = 2 * (integerLog2 m + errorBits)
  --                 in boundErrorTerm $ Approx
  --                        (round (unsafeShiftL m s'%(d)))
  --                        (ceiling (1%2 + unsafeShiftL e s'%(d)))
  --                        (-s-s')
  | otherwise = Bottom

-- |Divide an approximation by an integer.
divAInteger :: Approx -> Integer -> Approx
divAInteger Bottom _ = Bottom

divAInteger (Approx mb m e s) n =
  let
    t = integerLog2 n
  in
    approxMB mb
      (roundRational (shift m t % n))
      (ceilingRational (shift e t % n))
      s

-- |Compute the modulus of two approximations.
modA :: Approx -> Approx -> Approx
modA (Approx mb1 m e s) (Approx mb2 n f t) =
  let
    r = min s t

    (Tuple d m') = divMod (shift m (s - r)) (shift n (t - r))

    e' = scale e (s - r) + abs d * scale f (t - r)
  in
    approxMB2 mb1 mb2 m' e' r

modA _ _ = Bottom

-- |Compute the integer quotient (although returned as an 'Approx' since it
-- may be necessary to return 'Bottom' if the integer quotient can't be
-- determined) and the modulus as an approximation of two approximations.
divModA :: Approx -> Approx -> (Tuple Approx Approx)
divModA (Approx mb1 m e s) (Approx mb2 n f t) =
  let
    r = min s t

    (Tuple d m') = divMod (shift m (s - r)) (shift n (t - r))

    e' = e + abs d * f
  in
    Tuple (fromInteger d) (approxMB2 mb1 mb2 m' e' r)

divModA _ _ = Tuple Bottom Bottom

{-|
This function bounds the error term of an 'Approx'.

It is always the case that @x `'better'` boundErrorTerm x@.

Consider an approximation @Approx mb m e s@. If @e@ has /k/ bits then that
essentially expresses that the last /k/ bits of @m@ are unknown or garbage. By
scaling both @m@ and @e@ so that @e@ has a small number of bits we save on
memory space and computational effort to compute operations. On the other
hand, if we remove too many bits in this way, the shift in the mid-point of the
interval becomes noticable and it may adversely affect convergence speed of
computations. The number of bits allowed for @e@ after the operation is
determined by the constant 'errorBits'.

== Domain interpretation and verification

For this implementation to be correct it is required that this function is
below the identity on the domain /D/ of 'Approx'. For efficiency it is
desirable to be as close to the identity as possible.

This function will map a converging sequence to a converging sequence.
-}
boundErrorTerm :: Approx -> Approx
boundErrorTerm Bottom = Bottom

boundErrorTerm a@(Approx mb m e s)
  | e < errorBound = a
  | otherwise =
    let
      k = integerLog2 e + 1 - errorBits

      t = testBit m (k - 1)

      m' = shift m (-k)

      -- may overflow and use errorBits+1
      e' = shift (e + bit (k - 1)) (-k) + one
    in
      if t then
        Approx mb (m' + one) e' (s + k)
      else
        Approx mb m' e' (s + k)

boundErrorTermMB :: Approx -> Approx
boundErrorTermMB Bottom = Bottom

boundErrorTermMB a@(Approx _ m e s)
  | e < errorBoundMB = a
  | otherwise =
    let
      k = integerLog2 e + 1 - errorBitsMB

      t = testBit m (k - 1)

      m' = shift m (-k)

      -- may overflow and use errorBits+1
      e' = shift (e + bit (k - 1)) (-k) + one
    in
      if t then
        approxAutoMB (m' + one) e' (s + k)
      else
        approxAutoMB m' e' (s + k)

{-|
Limits the size of an approximation by restricting how much precision an
approximation can have.

It is always the case that @x `'better'` limitSize x@.

This is accomplished by restricting the exponent of the approximation from
below. In other words, we limit the precision possible.

It is conceivable to limit the significance of an approximation rather than
the precision. This would be an interesting research topic.

== Domain interpretation and verification

For this implementation to be correct it is required that this function is
below the identity on the domain /D/ of 'Approx'. For efficiency it is
desirable to be as close to the identity as possible.

This function will NOT map a converging sequence to a converging sequence for
a fixed precision argument. However, if the function is applied with
increasing precision for a converging sequence, then this will give a
converging sequence.
-}
limitSize :: Precision -> Approx -> Approx
limitSize _ Bottom = Bottom

limitSize l a@(Approx mb m e s) = result
  where
  k = (-s) - l

  result
    | k > 0 =
      Approx (mb - k)
        ((if testBit m (k - 1) then (_ + one) else identity) (shift m (-k)))
        (one + (shift (e + bit (k - 1)) (-k)))
        (-l)
    | otherwise = a

-- |Bounds the error term and limits the precision of an approximation.
--
-- It is always the case that @x `'better'` limitAndBound x@.
limitAndBound :: Precision -> Approx -> Approx
limitAndBound limit = limitSize limit <<< boundErrorTerm

limitAndBoundMB :: Precision -> Approx -> Approx
limitAndBoundMB limit = limitSize limit <<< boundErrorTermMB

-- | Find the hull of two approximations.
unionA :: Approx -> Approx -> Approx
unionA Bottom _ = Bottom

unionA _ Bottom = Bottom

unionA a@(Approx mb1 _ _ _) b@(Approx mb2 _ _ _) = endToApprox (mb1 `max` mb2) (lowerBound a `min` lowerBound b) (upperBound a `max` upperBound b)

increasingFunctionViaBounds :: (Approx -> Approx) -> (Approx -> Approx)
increasingFunctionViaBounds f a = (f $ lowerA a) `unionA` (f $ upperA a)

increasingPartialFunctionViaBounds :: (Approx -> Maybe Approx) -> (Approx -> Maybe Approx)
increasingPartialFunctionViaBounds f a = do
  resultL <- f $ lowerA a
  resultU <- f $ upperA a
  pure $ resultL `unionA` resultU

{-|
Second argument is noise to be added to first argument. Used to allow for the
error term when truncating a series.
-}
fudge :: Approx -> Approx -> Approx
fudge a (Approx _ m e _)
  | m == zero && e == zero = a

fudge (Approx mb m e s) (Approx mb' m' e' s')
  | e == zero = approxMB2 mb mb' (m `shift` (s - s')) (abs m' + e' + one) s'

fudge (Approx mb m e s) (Approx mb' m' e' s') =
  let
    m'' = one + (abs m' + e') `shift` (s' - s + 1)
  in
    approxMB2 mb mb' m (e + m'') s

fudge _ _ = Bottom
