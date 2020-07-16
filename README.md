# SimTeach
This repository contains the code for example simulations included in the paper "Simulation as a tool for teaching and learning epidemiologic methods"

The first ("simteach_non-diff-misclass.R") explores the direction of the bias under different non-differential misclassification scenarios:
1) When only a binary variable is non-differentially misclassified
2) When only a continuous variable is non-differentially misclassified
3) When both variables are misclassified and the errors are independent
4) When both variables are misclassified and the errors are dependent

The second ("simteach_p-value.R") uses simulation to generate the distribution from which p-values are calculated to help explain the definition of a p-value.

We also include code for a non-epidemiologic example no longer included in the paper that uses the formula for the area of a circle and randomly drawn points on the x,y coordinates to estimate the value of pi.
