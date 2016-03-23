Stochastic Simulation Algorithm in Haskell
==========================================

A simple Haskell implementation of the famous Gillespie algorithm. Models are
specified using a format similar to the SHAVE format:

    parameters
      ka = 0.2
      kb = 0.4

    species
      A : int
      B : int

    reactions
      A -> 2 B @ ka * A;
      2 B -> A @ kb * B;

    init
      A = 10
      B = 0
