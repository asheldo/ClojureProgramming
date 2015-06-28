E.g. load utils-union and the algo1-qu-nnn code and from the algo1-qu-nnn namespace:
(def at (a-maketestdata 5000 5 200000)) 

This will set at to the atom with results. 

Process also prints out (takes) the first 5 pairs for to show you. Look at the first pairs in @at. 

To see more connections of first member of a pair, e.g. 42:
(take 1  (for [a [42] b (range 0 200000) :when (and (not= a b) (a-connected @at a b)] [a b]))
