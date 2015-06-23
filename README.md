# NEET
NEET is a Haskell library for evolving NEAT neural networks. I wrote it because I saw
MarI/O (https://www.youtube.com/watch?v=qv6UVOQ0F44), a neat application of NEAT to playing
Super Mario World. I plan on using this package to mess around with AI.

## Features
- Lots of parameters
- Training networks
- Using networks

## Planned Features
- Serialization
- Better rendering
- Training in an arbitrary monad
- Split the parameters up

## Lofty Dreams
- CPPN and HyperNEAT support

## What is NEAT?
NEAT, or Neuroevolution of Augmenting Topologies, is a genetic algorithm for evolving
neural networks. When NEAT was developed, its novel use of historical markers for the
genes encoding neural connections allowed it to "cross over" those genes (like in real
meiosis), while
sidestepping problems caused by the fact that different neural networks could use
similar connections for different purposes. Instead of trying to analyze the network's
shape, the algorithm simply matches up genes with the same ID, and crosses those over.

Historical markers also make it easier to group different networks into species, as
the markers provide a genetic record that can be used to determine relatedness of two
genomes. Being able to speciate is beneficial, as it prevents the population from
converging on a non-optimal solution as easily. This is possible because organisms
compete primarily within their own species, which maintains genetic diversity and in
turn allows the development of several approaches to a problem.

The diversity provided by species itself enables NEAT to start with a minimal network
(usually a fully connected network of only inputs and outputs), as it can build
diversity up. At the time NEAT was published, previous genetic neuroevolution algorithms instead
started networks with more neurons and connections to generate an initial pool of
diversity. This can slow down the learning process, as more weights need to be tuned,
and the extra complexity might not have been necessary for a solution anyway.
