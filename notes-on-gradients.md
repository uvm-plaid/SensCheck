
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