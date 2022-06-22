# solo-verify

This is a library to verify that a Solo annotation is correct.

## How to run
```bash
stack test --fast
```

## TODOs
* Depend on solo library instead of copying and pasting code (might be difficult since I've had trouble compiling dduo repo on an M1)
* remove template-haskell-utils

## Notes from June 17

-- Matrix.Matrix (SDouble Diff s1)   == a matrix of sensitive doubles where each element has sensitivity s1
-- SMatrix L1 (SDouble Diff) s1      == a matrix of sensitive doubles with L1 sensitivity s1

-- [SDouble Diff s1]  == a list of sensitive doubles, each with the sensitivity env s1
-- almost always, lists have sensitivity tracked by a list-level metric (L1 or L2)

-- Think of a database of individuals, where one row = one person
-- The database can be represented as a list
-- Neighboring lists differ in one person's data (i.e. one element) but we don't know which one
-- This means neighboring lists have L1 distance of 1

-- For [SDouble Diff s1], distance between [1,2,3] and [2,3,4] is [1,1,1]
-- For SList L1 s1, distance between [1,2,3] and [1,3,3] is 1
-- For SList L1 s1, distance between [1,2,3] and [2,3,4] is 3

-- for each argument type:
--  SDouble Diff _ => abs $ a1 - a2
--  SMatrix L2 _ => norm_2 $ a1 - a2
-- for the output:
--  use the same rules
-- for the property:
--  dout <= [[ sensitivity_expression ]]
--  s1 +++ s2 => d1 + d2
--  ScaleSens n s => n * d1

-- Types of arguments should be SMatrix L2 (SDouble Diff) '[]


## Notes from 06/02
what is the sensitivity of f?

f :: forall s1 s2. SDouble Diff s1 -> SDouble Diff s2 -> SDouble Diff (s1 +++ s2)
f a b = D_UNSAFE $ unSDouble x * unSDouble y

gen_SDouble :: forall s. SDouble Diff s -- NOT SURE AT ALL
gen_SDouble :: SDouble Diff '[] -- NOT SURE AT ALL
gen_SDouble = D_UNSAFE $ gen_double

Integration with solo: would require implementing quickcheck generators for Solo's sensitive types

unsafe_f :: Double -> Double -> Double
unsafe_f a b = a + b

Things to try, in order:
1. unsafe_f only, don't deal with any Solo types at all
2. "shallow integration":
   - add gen_SDouble with non-descriptive sensitivity annotation ('[])
   - modify the prop to deal with solo types (extract regular doubles from SDoubles)
   - problem: we don't check that actual type annotation matches the property
3. "deep integration":
   - ????????
   - maybe: type-level functions that generate quickcheck properties based on type annotations given to solo functions

gen_prop @f <--- is a type level function that extracts f's type annotation and generates a quickcheck property which asserts f's sensitivity annotation is correct

how can we test if this is the right sensitivity annotation?

1. generate pairs of inputs
2. run f on each input
3. compare distances between inputs and outputs

Two hypothetical worlds = two hypothetical datasets

a1 = 5
a2 = 6
d(a1, a2) = | a1 - a2 | = 1

b1 = 1
b2 = 3
d(b1, b2) = | b1 - b2 | = 2

f a1 b1 = 5 * 1 = 5
f a2 b2 = 6 * 3 = 18

d(f(a1,b1), f(a2,b2)) = | 5 - 18 | = 13


Question: are these distances consistent with the annotation, or is this set of values a counterexample to the annotation?

Our annotation is:

 if:
  - distance between a1 and a2 is s1 (= 1)
  - distance between b1 and b2 is s2 (= 2)
 then:
  - distance between f(a1, b1) and f(a2, b2) <= s1 + s2 (= 3 ! <= 13)


Two phases:
1. check a sensitivity annotation against an implementation (requires someone to write the annotation)
2. synthesize a sensitivity annotation based on the implementation

prop a1 a2 b1 b2 =
  let d1 = abs (a1 - a2)
      d2 = abs (b1 - b2)
      dout = abs $ (unsafe_f a1 b1) - (unsafe_f a2 b2)
  in d1 + d2 <= dout

