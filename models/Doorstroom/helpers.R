library(zoo)

## Ignore NA's in cumulative functions
cumfunc_na.rm <- function(x, cumfunc) {
  ## Assert cumfunc is in vector of allowed functions
  approved_functions <- c(cumsum, cummean, cumany, cummin, cummax)
  assert(any(map_lgl(approved_functions, ~identical(.x, cumfunc))))

  ## Use previos value instead of NA, keep leading NA's
  x <- replace(x, !is.na(x), cumfunc(x[!is.na(x)]))
  x <- na.locf(x, na.rm = FALSE)
  return(x)
}
