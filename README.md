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

## How to build this repo

First install blas and lapack

**Ubuntu**

```bash
sudo apt-get install libblas-dev liblapack-dev
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



