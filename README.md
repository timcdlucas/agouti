

agouti
======

<!-- badges: start -->
[![R-CMD-check](https://github.com/timcdlucas/agouti/workflows/R-CMD-check/badge.svg)](https://github.com/timcdlucas/agouti/actions)
<!-- badges: end -->


AGgregate OUtcomes Ish. 

Aggregate outcomes are response data that are aggregated over various spaces. 
Other related names are disaggregation regression, and downscaling models.

This package is in the very early stages of development. There's barely anything here.

A clear-cut example is spatial models where covariates are measures at high resolution while responses (such as disease cases) are aggregated at the country or state level.
However, we can consider aggregation in other ways. 
Someone experiencing air pollution or another environmental exposure might be diagnosed with a disease only once. But that disease progression was an aggregate effect of all their previous risk.
Mortality counts might be aggregated at the monthly level, but deaths actually occur on the second or minute time-scale.

These models typically have an unusual data structure where there are more rows of covariate data than there are rows of response data. 
Therefore, even basic models can be difficult to code up. 
This package aims to make the basic workflow of modelling with aggregate outputs easier.
Furthermore, we hope that other packages that provide more complex analyses can depend on this package.



