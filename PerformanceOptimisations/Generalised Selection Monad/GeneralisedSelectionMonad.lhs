
> {-# LANGUAGE ImpredicativeTypes #-}
> {-# LANGUAGE ScopedTypeVariables #-}


> import Prelude hiding ((>>=), return, pure, (<*>), fmap, sequence, Left, Right)

Generalised Selection monad
=============================

Abstract
========

General setup and introduction words here....

Introduction to the Selection Monad
===================================

This section introduces the selection monad, focusing on the `type J r a = (a -> r) -> a`
for selection functions. The `pair` function is explored, showcasing its capability to 
compute new selection functions based on criteria from two existing functions. Illustrated 
with a practical example, the decision-making scenarios involving individuals navigating 
paths underscore the functionality of selection functions.

An analysis of the inefficiency in the original `pair` function identifies redundant computational 
work. The primary contribution of the paper is then outlined: an illustration and proposal 
for an efficient solution to enhance `pair` function performance. This introductory overview 
sets the stage for a detailed exploration of the selection monad and subsequent discussions 
on optimizations.

Selection functions
-------------------

Consider the tollowing already know type for selection functions:

> type J r a = (a -> r) -> a

When given two selection functions, a `pair` function can be defined to compute a new 
selection function. This resultant function selects a pair based on the criteria 
established by the two given selection functions:

> pair :: J r a -> J r b -> J r (a,b)
> pair f g p = (a,b)
>   where
>       a = f (\x -> p (x, g (\y -> p (x,y))))
>       b = g (\y -> p (a,y))

Example to illustrate the pair function
---------------------------------------

To gain a deeper understanding of the provided `pair` function, consider the following 
example. Picture two individuals walking on a path, one heading north and the other south. 
As they proceed, a collision is imminent. At this juncture, each individual must make a 
decision regarding their next move. This decision-making process can be modeled using 
selection functions.
The decision they need to make is modeled as either going right or left:

> data Decision = Left | Right

The respective selection functions decide given a predicate that tells them what decision 
is the correct one, select the correct one, and if there is no correct one, they default 
to walking right.

> p1, p2 :: J Bool Decision
> p1 p = if p Left then Left else Right
> p2 p = if p Left then Left else Right

To apply the `pair` function, a predicate `pred` is needed that will judge two decisions 
and return `True` if a crash would be avoided and `False` otherwise.

> pred :: (Decision, Decision) -> Bool
> pred (Left,Right) = True
> pred (Right,Left) = True
> pred _            = False

With the `pair` function, the merging of the two selection functions into a new one that 
identifies an optimal decision can now be calculated.

```
pair p1 p2 pred
--> (Left,Right)
```

Examining how the `pair` function is defined reveals that the first element `a` of the pair is 
determined by applying the initial selection function `f` to a newly constructed property 
function. Intuitively, selection functions can be conceptualized as entities containing a 
collection of objects, waiting for a property function to assess their underlying elements. 
Once equipped with a property function, they can apply it to their elements and select an 
optimal one.

Considering the types assigned to selection functions, it is evident that an initial 
selection function `f` remains in anticipation of a property function of type `(a -> r)` 
to determine an optimal `a`. The `pair` function is endowed with a property function 
`p :: ((a,b) -> r)`. Through the utilization of this property function, a property function 
for `f` can be derived by using the second selection function `g` to select a 
corresponding `b` and subsequently applying `p` to assess `(a,b)` pairs as follows:
`(\x -> p (x, g (\y -> p (x,y))))`. 
Upon the determination of an optimal `a`, a corresponding `b` can then be computed as 
`g (\y -> p (a,y))`.

In this case, the `pair` function can be conceptualized as a function that constructs 
all possible combinations of the elements within the provided selection function and 
subsequently identifies the overall optimal one.

It might feel intuitive to consider the following modified `pair` function that 
seems to be more symmetric.

> pair' :: J r a -> J r b -> J r (a,b)
> pair' f g p = (a,b)
>   where
>       a = f (\x -> p (x, g (\y -> p (x,y))))
>       b = g (\y -> p (f (\x -> p (x,y)), y))

However, applying this modified `pair'` to our previous example this results in a overall 
non optimal solution.

```
pair' p1 p2 pred
--> (Left,Left)
```

This illustrates how the original `pair` function keeps track of its first decision when 
determining its second element. It is noteworthy that, in the example example, achieving a 
satisfying outcome for both pedestrians is only possible when they consider the 
direction the other one is heading. The specific destination does not matter, as long 
as they are moving in different directions. Consequently, the original `pair` function 
can be conceived as a function that selects the optimal solution while retaining 
awareness of previous solutions, whereas our modified `pair'` does not.

An issue with the original `pair` function might have been identified by the attentive 
reader. There is redundant computational work involved. Initially, all possible pairs 
are constructed to determine an optimal first element `a`, but the corresponding `b` 
that renders it an overall optimal solution is overlooked, resulting in only `a` being 
returned. Subsequently, the optimal `b` is recalculated based on the already determined 
optimal `a` when selecting the second element of the pair.

The primary contribution of this paper will be to illustrate and propose a solution to 
this inefficiency.

Sequence
--------

The generalization of the pair function to accommodate a sequence of selection functions 
is the initial focus of exploration. In the context of selection functions, a sequence 
operation is introduced, capable of combining a list of selection functions into a 
singular selection function that, in turn, selects a list of objects:

> sequence :: [J r a] -> J r [a]
> sequence [] p     = []
> sequence (e:es) p = a : as
>   where 
>       a  = e (\x -> p (x : sequence es (p . (x:))))
>       as = sequence es (p . (a:))

Here, similar to the pair function, the sequence function extracts elements from the resulting 
list through the corresponding selection functions. This extraction is achieved by applying 
each function to a newly constructed property function that possesses the capability to 
foresee the future, thereby constructing an optimal future based on the currently examined 
element.

However, a notable inefficiency persists, exacerbating the issue observed in the pair function. 
During the determination of the first element, the sequence function calculates an optimal 
remainder of the list, only to overlook it and redundantly perform the same calculation for 
subsequent elements. This inefficiency in sequence warrants further investigation for potential 
optimization in subsequent sections of this research paper.

Selection monad
---------------

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

Example
-------

- TODO: give another example to ilustrate how we can model decisions with the selection monad
(Password example)


More efficient special K
------------------------

In order to adress this secific inefficiency of the selection monad with the
pair and sequence function we will introduce two new variations of the selection
monad. First, we will have a look at a new type K that will turn out to be 
isomorphic to the selection monad J. Then we will further generalise this K type
to be more intuitive to work whith. It turns out that the J monad can be embedded
into this genaralised K type. 

Special K
=========

Lets consider the following type K:

> type K r a = forall b. (a -> (r,b)) -> b

While selection functions of type J are still waiting for a predicate that
is able to judge its underlaying elements, the new K type works similar.
The predicate of the K type also judges its elements by turning them into r
values, but further also converts the x into any y, and returns that y along
with its judgement r.

> pairK :: K r a -> K r b -> K r (a,b)
> pairK f g p = f (\x -> g (\y -> let (r, z) = p (x,y) in (r, (r,z))))

- ilustrate on an example how that is more efficient
      - Basically because once it found a solution, the whole solution will be returned, and can be reused

- This is sequence for the new K type.

> sequenceK :: [K r a] -> K r [a]
> sequenceK [e] p    = e (\x -> p [x])
> sequenceK (e:es) p = e (\x -> sequenceK es (\xs -> let (r,y) = p (x:xs) in (r,(r,y))))

- state that it has the same efficiency advantages

Special K isomorphic to J
-------------------------

- Give k2j and j2k

> k2j :: K r a -> J r a
> k2j f p = f (\x -> (p x, x)) 

> j2k :: J r a -> K r a
> j2k f p = snd (p (f (fst . p)))

- intoduce free theorem for special K
- proof that they are isomorphic

- End with a final point that this is complicated to deal with! Lots of unpacking

Generalised K
-------------

- what we really want is the generalised K

> type GK r x = forall y. (x -> (r,y)) -> (r,y)

- give the intuitive monad definition for new K

> bindGK :: GK r a -> (a -> GK r b) -> GK r b
> bindGK e f p = e (\x -> f x p)

> returnGK :: a -> GK r a
> returnGK x p = p x

- give pair and sequence

> pairGK :: GK r a -> GK r b -> GK r (a,b)
> pairGK f g p = f (\x -> g (\y -> p (x,y)))

> sequenceGK :: [GK r a] -> GK r [a]
> sequenceGK [e] p    = e (\x -> p [x])
> sequenceGK (e:es) p = e (\x -> sequenceGK es (\xs -> p (x:xs)))

- ilustrate how nice it is to deal with

Relationship to J and Special K
-------------------------------

- Show that generalised K is an embedding

> k2gk :: K r a -> GK r a
> k2gk f = snd . f


> gk2k :: GK r a -> K r a
> gk2k f p =  f (\x -> let (r,y) = p x in (r, (r,y)))

- intoduce free theorem and precondition
- counterexamples to ilustrate what precondition means and why we want it
- introduce new theorem baced on free theorem and precondition
- calculate monad definition from k2j and j2k

Performance analisys
====================

-  give some perfomance analysis examples that ilustrate improvement

Related work
============

J was researched in the context of Sequential games, but slowly found its way to other 
applications

Outlook and future work
=======================

- Need to investigate further whats possible with the more general type
- Alpha beta pruning as next step of my work

Conclusion
==========

- We should use generalised K istead of J because more useful and more intuitive 
  once understood
- performance improvements are useful
- monad pair and sequence implementation much more intuitive and useful

Appendix
========

Proofs!