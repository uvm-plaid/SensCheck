
So I need to take [Gradients Layers] -> Gradients Layers

In other words a bunch of gradients into a single gradient

There are many types of layers
For example Convolution is representated by a data type that has weights and biases
It does a vector multiplication when running forward.

I can see it uses the linear algebra library to do that.

Gradient might have a value. How do I get that?
https://hackage.haskell.org/package/grenade-0.1.0/docs/Grenade-Core-Network.html#t:Gradients

Gradients is just a data type that represents a list of Gradient and has the UpdateLayer constraint

Maybe UpdateLayer can give me something?

Gradient is an associated type to UpdateLayer
https://hackage.haskell.org/package/grenade-0.1.0/docs/Grenade-Core-Layer.html#t:Gradient

Unit if there isn't a Gradient

Thinking of this why am I going from a List of type level lists of Gradient to a type level list of Gradient

I should maybe go from Gradients Layers to Gradient Layers?


## Update trouble with Network

I am having trouble with the InceptionMini layer. This calls Network with pad and Convolution.

And complains about no Monoid Convolution which I don't want. I need a Convolution' which is what Gradient Convolution computes to.

Maybe the issue is with how the Gradient is defined on Network?
instance CreatableNetwork sublayers subshapes => UpdateLayer (Network sublayers subshapes) where
  type Gradient (Network sublayers subshapes) = Gradients sublayers
  runUpdate    = applyUpdate
  createRandom = randomNetwork

Ok well that defers to sublayers which seems sort of right...

But I think we are somehow missing a call to Gradient on the head of sublayer??

Turns out my monoid on Gradients was messing up things. Looking at that closer.
I shouldn't need that.
