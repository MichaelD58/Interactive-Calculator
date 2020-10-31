# Haskell 1 - Calculator

Calculator that supports a wide range of functions expected from an actual calculator; such as variable support, the handling of errors and an active
history.

## Execution
To run, open a ghci terminal and compile using ":l Main.hs", followed by "main". Terminal should display "0 >" or similar.
In order to use the QuickCheck functionality of BTTesting.hs, it is necessary to run the command "cabal install QuickCheck" at the terminal, and then from within ghci the commands ":l BTTesting.hs" and "quickCheck prop_ordered".
