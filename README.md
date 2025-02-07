# lbmstoolbox

This toolbox integrates and provides a unified wrapper for two widely-used length-based fisheries evaluation methods: 
[LB-SPR](https://github.com/AdrianHordyk/LBSPR) (Hordyk et al., 2015a, 2015b, 2016) and
[LIME](https://github.com/merrillrudd/LIME) (Rudd and Thorson, 2017). It also introduces several enhancements to improve 
usability and functionality, including:

1. **Streamlined Interface**: Both models now share a consistent interface, simplifying their use and integration.
2. **Full LB-SPR Features**: In addition to generating standard estimates, LB-SPR now re-runs its internal simulation 
function to provide detailed outputs on the estimated fished and unfished catch length composition, as well as the corresponding 
population length structure. These features were already available in the model however the wrapper exposes them as natural outputs.
3. **Modified LIME Features**: The version of [LIME](https://github.com/d2gex/LIME) used here has been forked and its TMB template modified to provide the unfished population
length structure.
5. Likewise, this wrapper on LIME provides the estimated fished catch length composition but not the unfished one. Furthermore,
both fished and unfished population length structures are now outputs in the age domain, reflecting LIME's fully age-structured 
modeling approach. While these features were already inherent to LIME, this wrapper makes them more accessible as standard outputs.
6. **Enhancing robustness of results**: Confidence intervals are now included with key evaluation metrics such as `SPR` and `FM` for LB-SPR and `SPR`, `F` 
and `R` for LIME.

## Caveats

1. All length classes in the simulation estimates for both models are truncated at the upper boundary to match the observed 
length classes. Note that this adjustment may affect model outputs, as model fits typically extend beyond the observed limits.
2. Though the wrapper works for any time-step, the name of the column identifying this variable is named `years`. Likewise
for the length variable the name of the column is `MeanLength`.

## Installation
This R package can be installed through the devtools as follows:
```r 
  devtools::install_github("https://github.com/d2gex/lbmstoolbox", dep=TRUE)
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
