# Causality-test

Using convergent cross mapping (Sugihara et al. Science, 2012) we can test for the existence of causal relations among variables within nonlinear systems

The code is organized as follow:

1) Find the optimal embedding dimension
2) Run convergent cross mapping with optimal embedding dimension
3) Create surrogate time series and test for causality of the randomized version of the data

To ensure significance of a causal link cross mapping skills need to increase with the number of data points and need to be higher than the cross mapping skills observed in the surrogate time series
