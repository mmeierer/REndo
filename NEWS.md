# CHANGES IN REndo 2.1.0

## SIGNIFICANT USER-VISIBLE CHANGES
* The reworked method `multilevelIV` and accompanying data was added back to the package
* Vignette `REndo-introduction` was added to showcase package usage
* Users can supply a parameter `optimx.args` to tweak the LL optimization to their liking

## NEW FEATURES
* Method `confint` was added for methods `latentIV` and `copulaCorrection`
* Examples and documentation were improved for all methods
* New data was generated for dataHetIV
* The default number of iterations for all optimizations was increased to 100'000

## BUG FIXES
* To avoid infrequent warnings, the parameter `sigma` used in `copualCorrection` was constrained to > 0
* Various spelling mistakes were fixed



# CHANGES IN REndo 2.0.0

## SIGNIFICANT USER-VISIBLE CHANGES
* Remodeled all methods' user-interface
* Added detailed input checks for every provided parameter
* Adapted all visible outputs
* Parameter `verbose` allows to turn on or off printing
* Updated documentation to reflect all changes and added theoretical background

## NEW FEATURES
* Formulas support transformations
* Improve all code to be more reliable and stable
* Provide new example datasets and accompanying documentation for all methods
* Added extensive testing for all aspects of the package
* Increased numerical stability for log-likelihood optimization methods `latentIV` and `copualCorrection`

## BUG FIXES
* Many

# DEPRECATED AND DEFUNCT
* The multilevel function is temporarily removed from the package due to ongoing work on it
