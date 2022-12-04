# solo-verify

## Short description
This is a library to verify that a Solo annotation is correct.

## How to use this library

1. Define functions with solo type signatures. e.g. `f :: SDouble Diff s1 -> SDouble Diff s2 -> SDouble Diff (s1 +++ s2)`
2. Generate quickcheck tests for your function with `genMainQuickCheck` also providing it with the main function you would like to generate.
3. Call the function in your test.

### Example

test/Spec.hs
```
{-# LANGUAGE TemplateHaskell #-}

-- Generates a function tests :: IO () that execute a generated quickcheck tests for f and any other functions
$(genMainQuickCheck "tests" ['f])

main :: IO ()
main = tests
```

## How to run this repo
```bash
stack test --fast
```

## Longer Description

### Problem

Solo allows you to create differentially private algorithms that will calculate sensitivity environments at the type level.
Solo exposes primatives such as a Double, List, Tuple, and other common types that are compatable with Solo.
Solo also exposes common functions that Haskellers are familar with such as fmap, bind, and filter.


However as a developer attempting to write a differentially private algorithm I may need much more than the common utility functions that are provided.
I may write my own Solo compatable functions, or I may want to use existing libraries such as to do Matrix computations.
I could just write my own functions or use those external libraries and re-wrap it with Solo types.


**I could be incorrect in my assumptions about the types of functions**

For example, here is a matrix addition function that is wrapped with Solo types. I will come back to this example:

```haskell
add_matrix_solo :: SDoubleMatrixL2 s1 -> SDoubleMatrixL2 s2 -> SDoubleMatrixL2 (s1 +++ s2)
add_matrix_solo m1 m2 =
  SMatrix_UNSAFE $
    D_UNSAFE
      <$> (unSDouble <$> unSMatrix m1) + (unSDouble <$> unSMatrix m2)
```

The Matrix is provided by an external library and this is a correct implementation of the type.
The 2 sensitivities provided are added at the type level. However the developer could easily forget to forget to add the sensitivies.

### Solution

It is possible to write a property based test just by looking the type signature. Therefore it is possible **automatically generate the same test** that could be manually written.
This would allow you to validate your implementation without any additional work.

Here is a proper property that can be used by quickcheck to validate our implementation:

```haskell
prop_safe_add_solo a1 a2 b1 b2 =
   -- L2 distance between first arguments
  let d1 = norm_2 $ toDoubleMatrix a1 - toDoubleMatrix a2 
      d2 = norm_2 $ toDoubleMatrix b1 - toDoubleMatrix b2
      -- L2 distance between two outputs
      dout = norm_2 $ toDoubleMatrix (add_matrix_solo a1 b1) - toDoubleMatrix (add_matrix_solo a2 b2)
   in dout <= d1 + d2 + 0.000000001 -- Add a bit of padding since due to floats aren't completely accurate.
```

### Writing a quickcheck test

Let's walk line by line about how a person or program could write the above test.

```haskell
prop_safe_add_solo a1 a2 b1 b2 =
```

We have a function that takes 2 inputs and we need 2 inputs per input.
We can tell the number of inputs from the type signature of `add_matrix_solo`.

```haskell
let d1 = norm_2 $ toDoubleMatrix a1 - toDoubleMatrix a2 
    d2 = norm_2 $ toDoubleMatrix b1 - toDoubleMatrix b2
```

We need to calculate the distance between inputs. The distance function we use here is the L2 Norm of a Vector or Matrix in our case.
Here a1 has the same type as a2 and b1 has the same type as b2.

```haskell
dout = norm_2 $ toDoubleMatrix (add_matrix_solo a1 b1) - toDoubleMatrix (add_matrix_solo a2 b2)
```

Now we need to calculate the distance between two calls of the function. Again using the L2 Norm function.

```haskell
in dout <= d1 + d2 + 0.000000001 -- Add a bit of padding since due to floats aren't completely accurate.
```

Our assertion is that the distance between inputs should always be greater or equal to the distance of the output of the function.



----

Unstructured notes

## Other solutions

-- 1. COuld write my own quasi quoter
-- 2. Take over a file
-- 3. Take over many files
-- Do somethign like hspec discover preprocessor

-- Example quasiquoter
[gen| f :: SDouble Diff s1 -> SDouble Diff s2 -> SDouble Diff (s1 +++ s2) |]

## TODOs

* Figure out how to use the generated props
* Depend on solo library instead of copying and pasting code (might be difficult since I've had trouble compiling dduo repo on an M1) (Not important?)
* remove template-haskell-utils
* Add absdist diff somewhere https://github.com/uvm-plaid/pbt-sensitivity/pull/4#issuecomment-1165632399
* How do we scan a file and then run our template haskell functions
* Alternatively we can explicity call our genProp function. Downside: developer can forget something. Also more work for devs.
* We decided to generate the quickcheck tests and let it fail. In the paper we will talk about how you need dependent types to make thsi work nciely.
* That also means we should make arbitrary instances for SMatrix and SList.
* Write dependently typed Matrix 



## Notes July 7

There are some functions with sensitvities encoded in the types

e.g. SDouble Diff (ScaleSens 3 s1)
Could be input and/or output I think
But maybe not worth thinking about?
The places where it shows up.

Would be a question for the paper. What do we do about higher order functions.
Can we have quick check generate functions?? Don't think it's possible.

Maybe we really can only test against concrete functions and in that case it simplifies the type

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

---- Notes 08.04

prop_distance_solo :: SDouble Diff '[] -> SDouble Diff '[] -> SDouble Diff '[] -> SDouble Diff '[] -> Bool
prop_distance_solo a1 a2 b1 b2 =
  let d1 = abs $ unSDouble a1 - unSDouble a2
      d2 = abs $ unSDouble b1 - unSDouble b2
      dout = abs $ unSDouble (f a1 b1) - unSDouble (f a2 b2)
   in dout <= d1 + d2 + 0.000000001

a1 and a2 should be same type (A)
b1 and b2 should be same type (B)


let's say f does (s1 ++ s2)

d1 is associated to s1 or type A which contains s1
d2 is associated to s2 or type B which contains s2


d1 is computed from all type As
d2 is computed from all type Bs

so my parsing logic needs all distance variables and associated sensitivty value.
Then do a reverse lookup to retrieve the distance variable given the sensitivity value.

I associate distance variables by getting the term of the first thing then getting the s env from it

-- TODO not sure what to do with this
f:: SDouble Diff s1 -> SDouble Diff s1 -> SDouble Diff (s1 +++ s1)
-- This would generate 2 distance statements with the s1 associated to it.
-- Potential solution use whatever comes in order.
-- So create a Map SEnv [DistanceName]. Where the [DistanceName] is ordered by occurance.
-- When I encounter an SEnv. Do a lookup and pop from the List.
-- Then recursively call with that removed DistanceName.


TODO just realized you can have a distance with multiple SEnv.


f:: SDouble Diff (s1 +++ s2) -> SDouble Diff (s3 +++ s4) -> SDouble Diff ((s1 +++ s2) +++ (s3 +++ s4))
-- This completely screws up computeRhs. Opinion just ignore it for now?




