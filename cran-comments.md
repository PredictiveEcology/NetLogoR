## Updated release

This release is to restore this package on CRAN following removal of dependencies `reproducible`, `SpaDES.tools`, and `SpaDES.core` (which have now been restored on CRAN).
We have also improved the documentation, and added return values for most functions.
See `NEWS.md` for a full list of changes.

## Test environments

### Previous R versions
* Ubuntu 20.04                 (GitHub), R 3.6.3
* Ubuntu 20.04                 (GitHub), R 4.0.5
* Windows                      (GitHub), R 3.6.3
* Windows                      (GitHub), R 4.0.5
* Windows                 (win-builder), R 4.0.5

### Current R versions
* macOS 10.15.7 Catalina       (GitHub), R 4.1.0
* macOS 11.1 Big Sur            (local), R 4.1.0
* Ubuntu 20.04                 (GitHub), R 4.1.0
* Ubuntu 20.04                  (local), R 4.1.0
* Windows                      (GitHub), R 4.1.0
* Windows                       (local), R 4.1.0
* Windows                 (win-builder), R 4.1.0

### Development R version
* Ubuntu 20.04                 (GitHub), R-devel (2021-06-09 r80471)
* Ubuntu 20.04                  (local), R-devel (2021-06-18 r80526)
* Windows                      (GitHub), R-devel (2021-06-09 r80471)
* Windows                 (win-builder), R-devel (2021-06-07 r80458)

## R CMD check results

There were no ERRORs or WARNINGs.

There was 1 NOTE:

1. The URL given to install `fastshp` is in quotes in DESCRIPTION as it is part of a command.

## Downstream dependencies

There are currently no downstream dependencies of this package.
