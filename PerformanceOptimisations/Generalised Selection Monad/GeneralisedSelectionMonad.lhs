
> {-# LANGUAGE ImpredicativeTypes #-}
> {-# LANGUAGE ScopedTypeVariables #-}


> import Prelude hiding ((>>=), return, pure, (<*>), fmap, sequence, Left, Right)

- Generalised Selection monad

General setup and introduction words here....

-- Introduction to the selection monad
--- General Idea 
 
Consider the tollowing already know type for selection functions:

> type J r a = (a -> r) -> a

Given 2 selection functions we can define a pair function that 
computes a new selection function that selects a pair based on the 
two given selection functions:

> pair :: J r a -> J r b -> J r (a,b)
> pair f g p = (a,b)
>   where
>       a = f (\x -> p (x, g (\y -> p (x,y))))
>       b = g (\y -> p (a,y))

--- Example to ilustrate pair

To understand this pair function better consider the following example.
Imagine two people walking on a path, one walking north and one walking south.
When both were continuing on their path, they would crash into eachother.
They both now need to make a decition about what to do next. We can model this
decision as selection functions:

We model their decision they need to make as either go right or left

> data Decision = Left | Right 

The respective selection functions decide given a predicate that tells them what
decision is the correct one, select the correct one, and if there is no correct one
they default to walking right

> p1, p2 :: J Bool Decision
> p1 p = if p Left then Left else Right
> p2 p = if p Left then Left else Right

In order to apply our pair function we need a predicate p that will judge
two decisions and returns True if they would avoid a crash:

> pred :: (Decision, Decision) -> Bool
> pred (Left,Right) = True
> pred (Right,Left) = True
> pred _            = False

With our pair function we can now combine the two selection functions into a new
one that selects an optimal decision:

pair p1 p2 pred
--> (Left,Right)

Looking at how pair is defined, we can see that the first element a of the pair is 
determined by applying the first selection function f to a newly constructed property
function. Intuitively we can think of selection functions as functions that already
contain a collection of objects, and are just waiting for a property function that 
judges its underlaying elements. With that property function it can then apply the 
property function to its elements and select an optimal one.

From the type for selection functions we know that our first selection function f 
is still waiting for a property function of type (a -> r) to decide on an optimal a.
The pair function is given a proerty function p :: ((a,b) -> r). With that property
function we are able to build a property function for f by using the second selection
function to select an corrisponding b and then use p to judge (a,b) pairs like this:
(\x -> p (x, g (\y -> p (x,y))))
Once we decided on an optimal a we can then compute a corrisponding b like this:
g (\y -> p (a,y))

In this case we can therefore think of the pair function as a function builds all
possible combinations of the the elements of the given selection function and then 
selects the overall optimal one.

It might feel intuitive to consider the following modified pair function that
appears to be more symetric

> pair' :: J r a -> J r b -> J r (a,b)
> pair' f g p = (a,b)
>   where
>       a = f (\x -> p (x, g (\y -> p (x,y))))
>       b = g (\y -> p (f (\x -> p (x,y)), y))

However, applying this to our previous example this results in a overall non optimal 
solution

pair' p1 p2 pred
--> (Left,Left)

This ilustrates how the original pair function is keeping track of its first decision
when deciding its second element. Note that in our example it is only possible to 
arrive at a satisfing outcome for both pedestriants when they take into account where 
the other one is going. It dosent matter where they are going as long as they are going
into different directions. We can therefore think of the original pair function as a 
function that selects the optimal solution while keeping track of previous solutions,
while our modified pair' does not. 

The careful reader might have spotted an issue with the original pair function. Some 
computational work will be done twice. We build all possible pairs to
decide on an optimal first element a, but then forget about the corrisponding b that 
makes it an oveall optimal solution and just return the a. And then recalculate it 
again based on the already optimal a when selecting the second element of the pair.

The main contibution of this paper will be to ilustrate and to provide a solution to 
this inefficiency.

--- Sequence
But first, lets have a look how we can generalise the pair function to for a sequence
of selection functions.

Sequence for selection functions is taking a list of Selection functions and combines
them into a single selection function that selects a lost of objects:

> sequence :: [J r a] -> J r [a]
> sequence [] p     = []
> sequence (e:es) p = a : as
>   where 
>       a  = e (\x -> p (x : sequence es (p . (x:))))
>       as = sequence es (p . (a:))

Similar to the pair function, sequence extracts elements of the resulting list
out of the given corrisponding selection function. It does so by applying it 
to a newly constructed property function that is able to look into the future,
building up a optimal future based on the element it currently is looking at.

Here, the the same inefficiency is even more of a problem. When deciding its 
first element, it already calculated an optimal rest of the list, but forgets
about it and just moves on doing the same calculation again for the second element.


--- Selection monad

Selection functions are forming a monad in the following way:

> (>>=) :: J r a -> (a -> J r b) -> J r b
> (>>=) f g p = g (f (p . flip g p)) p

> return :: a -> J r a
> return x p = x

The haskell standard library already has a build-in sequence functon for 
monads:

> sequence' :: [J r a] -> J r [a]
> sequence' (ma:mas) = ma >>= \x -> sequence' mas >>= \xs -> return (x:xs)

Which in the case for the selection monad is equivalent to the previous given
sequence implementation. 

- TODO: give another example to ilustrate how we can model decisions with the selection monad
(Password example)


-- More efficient special K
In order to adress this secific inefficiency of the selection monad with the
pair and sequence function we will introduce two new variations of the selection
monad. First, we will have a look at a new type K that will turn out to be 
isomorphic to the selection monad J. Then we will further generalise this K type
to be more intuitive to work whith. It turns out that the J monad can be embedded
into this genaralised K type. 

--- Special K

Lets consider the following type K:

> type K r x = forall y. (x -> (r,y)) -> y

While selection functions of type J are still waiting for a predicate that
is able to judge its underlaying elements, the new K type works similar.
The predicate of the K type also judges its elements by turning them into r
values, but further also converts the x into any y, and returns that y along
with its judgement r.


- give pair implementation
- ilustrate on an example how that is more efficient
- give sequence implementation
- state that it has the same efficiency advantages

--- Special K isomorphic to J
- Give k2j and j2k
- intoduce free theorem for special K
- proof that they are isomorphic

- End with a final point that this is complicated to deal with! Lots of unpacking

-- Generalised K 
- what we really want is the generalised K
- give the intuitive monad definition for new K
- give pair and sequence
- ilustrate how nice it is to deal with

--- Relationship to J and Special K
- Show that generalised K is an embedding
- intoduce free theorem and precondition
- counterexamples to ilustrate what precondition means and why we want it
- introduce new theorem baced on free theorem and precondition
- calculate monad definition from k2j and j2k


-- Performance analisys
-  give some perfomance analysis examples that ilustrate improvement

-- related work
J was researched in the context of Sequential games, but slowly found its way to other 
applications


-- outlook and future work
- Need to investigate further whats possible with the more general type
- Alpha beta pruning as next step of my work

Conclusion
- We should use generalised K istead of J because more useful and more intuitive 
  once understood
- performance improvements are useful
- monad pair and sequence implementation much more intuitive and useful