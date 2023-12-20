.onLoad <- function(libname, pkgname) {
  # CRAN OMP THREAD LIMIT
  # From https://stackoverflow.com/questions/77323811/r-package-to-cran-had-cpu-time-5-times-elapsed-time
  Sys.setenv("OMP_THREAD_LIMIT" = 2)

}
