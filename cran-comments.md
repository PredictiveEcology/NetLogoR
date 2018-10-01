## Resubmission

In this version we have set a seed in tests to avoid random CRAN test failures.
We will ensure all seeds work in a future release. 

## Test environments

### Previous R versions
* Ubuntu 14.04        (travis-ci), R 3.3.0
* Ubuntu 14.04        (travis-ci), R 3.4.0
* Windows              (appveyor), R 3.3.0
* Windows              (appveyor), R 3.4.0
* Windows                 (local), R 3.4.4

### Current R versions
* macOS High Sierra    (local), R 3.5.0
* OS X El Capitan  (travis-ci), R 3.5.0
* Ubuntu 14.04     (travis-ci), R 3.5.0
* Ubuntu 18.04         (local), R 3.5.0
* Windows           (appveyor), R 3.5.0
* Windows        (win-builder), R 3.5.0
* Windows 7            (local), R 3.5.0

### Development R version
* Ubuntu 14.04     (travis-ci), R 3.6.0 (2018-06-05 r74851)
* Ubuntu 18.04         (local), R 3.6.0 (2018-06-05 r74852)
* Windows           (appveyor), R 3.6.0 (2018-06-05 r74852)
* Windows        (win-builder), R 3.6.0 (2018-06-05 r74852)

## R CMD check results

There were no ERRORs or WARNINGs

There was 1 NOTE:

1. The URL given to install `fastshp` is in quotes in DESCRIPTION as it is part of a command.

## Downstream dependencies

There are currently no downstream dependencies of this package.
