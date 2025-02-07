.onLoad <- function(libname, pkgname) {
  # Not the cleanest solution but required for object LB_pars to be exposed
  library(LBSPR)
  invisible()
}
