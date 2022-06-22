Might be difficult to synthezize quick check functions.



Idea
1. split by "arguments" and "return"
2. Create a function that will extract Senvs and their name
  1. This can be something like SDouble Diff s1 -> SDouble Diff s2
  I think I really need to parse something into a function F given arguments s1 s2 s3 = s1 ++ s2 op s3 
  so I guess form some AST 
  wait maybe I only care about the return type?
  But actually this doesn't completely map to a function because the "arguments" can be enclosed in things
  and also be added. e.g. s1 +++ s2 -> s3 -> s1 +++ s2 +++ s3

  Also what if an s env is discarded?

  Maybe I need a different AST for argument and return type?

  Ok subgoal: just parse the return type and form an AST
  We can refine from there.

  I wonder if arguments map to d1 and d2 from the below example
  and return maps to last statement.
  Not sure what maps to dout

-- Assert that |x1-x2| + |y1 - y2| <= |f(x1,y1) - f(x2, y2)|
prop_distance_solo a1 a2 b1 b2 =
  let d1 = abs $ unSDouble x1 - unSDouble x2
      d2 = abs $ unSDouble y1 - unSDouble y2
      dout = abs $ unSDouble (f x1 y1) - unSDouble (f x2 y2)
   in dout <= d1 + d2 + 0.000000001

f :: SDouble Diff s1 -> SDouble Diff s2 -> SDouble Diff (s1 +++ s2)
      s1 maps to x1 and x2 above
      s2 maps to y1 and y2 above


whoops I came to the same realization we already wrote mostly.
The output is dout. And I think we just take all the statements we have 

So we need a function that creates the type signature, the distance statements and distance output statement
and the property statement
