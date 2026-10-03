# lbmstoolbox

## LIME vignette

The LIME model-fitting chunks are currently disabled. A `Matrix` compatibility
issue can otherwise interrupt rendering with the error `function
'sexp_as_cholmod_sparse' not provided by package 'Matrix'`. The data-preparation
and parameter-setting chunks still run.

This toolbox provides a unified interface to two widely used length-based fisheries assessment methods:
[LB-SPR](https://github.com/AdrianHordyk/LBSPR) (Hordyk et al., 2015a, 2015b, 2016) and
[LIME](https://github.com/merrillrudd/LIME) (Rudd and Thorson, 2017). It also introduces several enhancements to improve 
usability and output access:

1. **Common interface:** Both models use the same high-level workflow.
2. **LB-SPR simulation output:** In addition to standard estimates, the wrapper runs LB-SPR's internal simulator and returns estimated fished and unfished catch-at-length and population-at-length structures. These outputs are available in LB-SPR but are exposed here as standard results.
3. **LIME simulation output:** The bundled fork of [LIME](https://github.com/d2gex/LIME) modifies its TMB template to return the unfished population structure. The wrapper also returns estimated fished and unfished catch-at-length and fished and unfished population-at-age structures.
4. **Uncertainty intervals:** Confidence intervals are returned for `SPR` and `FM` in LB-SPR, and for `SPR`, `F`, and `R` in LIME.

## Caveats

1. Simulation output for both models is truncated at the upper observed length boundary. This may affect results because fitted distributions commonly extend beyond the observed range.
2. The wrapper accepts any time step. In the long input data, the time-step column must be named `year` and the length-class column must be named `MeanLength`.

## Installation
Install the package with `devtools`:

```r
devtools::install_github("d2gex/lbmstoolbox", dependencies = TRUE)
```

## References

1. Hordyk, A., Ono, K., Sainsbury, K., Loneragan, N., Prince, J., 2015a. Some explorations of the life history ratios to 
describe length composition, spawning-per-recruit, and the spawning potential ratio. ICES Journal of Marine Science 72, 204–216. https://doi.org/10.1093/icesjms/fst235
2. Hordyk, A., Ono, K., Valencia, S., Loneragan, N., Prince, J., 2015b. A novel length-based empirical estimation method of 
spawning potential ratio (SPR), and tests of its performance, for small-scale, data-poor fisheries. ICES Journal of Marine Science 72, 217–231. https://doi.org/10.1093/icesjms/fsu004
3. Hordyk, A.R., Ono, K., Prince, J.D., Walters, C.J., 2016. A simple length-structured model based on life history ratios 
and incorporating size-dependent selectivity: application to spawning potential ratios for data-poor stocks. Can. J. Fish. Aquat. Sci. 73, 1787–1799. https://doi.org/10.1139/cjfas-2015-0422
4. Rudd, M.B., Thorson, J.T., 2017. Accounting for Variability and Biases in Data-limited Fisheries Stock Assessment. 
Canadian Journal of Fisheries and Aquatic Sciences 75, 1019–1035. https://doi.org/10.1139/cjfas-2017-0143
