library(data.table)
origDTthreads <- getDTthreads()
if (!identical(Sys.getenv("NOT_CRAN"), "true")) { # this asks "is this test running on CRAN"
  Sys.setenv("OMP_THREAD_LIMIT" = 1)
  withr::defer(
    {
      data.table::setDTthreads(origDTthreads)
    },
    teardown_env()
  )
}
setDTthreads() # this triggers data.table to read the OMP_THREAD_LIMIT
