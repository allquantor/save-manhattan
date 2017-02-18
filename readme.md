Problem:

Imagine a 2-d skyline of Manhattan was completely flooded (even the highest building). 
After the heavy rain stopped and the water started to flow out, some water is sucked between buildings and can't flow out. The task is to calculate the flooded area.

Constraints:
* Sun will come up soon, the calculation should be efficient before the water start to dry out :)
* All buildings are hermetically sealed - the building area should not be considered as flooded area.
* All buildings are boxed - so, the water can flow over the roofs.

Solution:

The given algorithm solves the problem in O(n) time. The whole trick is to beside of the accumulated
area, the state that define a so called "local area" should be considered. The local area contain
partial flooded area + the area of the buildings that should be subtracted from the global area if our 
algorithm decides that the sub set of graph is overflooded. 

Algorithm implementation can be found in

`SpaceLaserAreaCalculator.calculate(skyline)`

The skyline should be a type of `Seq[Buildings]`.

The tests can be found in `CalculationSpec`