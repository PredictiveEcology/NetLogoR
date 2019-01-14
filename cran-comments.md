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
* macOS High Sierra    (local), R 3.5.2
* OS X El Capitan  (travis-ci), R 3.5.2
* Ubuntu 14.04     (travis-ci), R 3.5.2
* Ubuntu 18.04         (local), R 3.5.2
* Windows           (appveyor), R 3.5.2
* Windows        (win-builder), R 3.5.2
* Windows 7            (local), R 3.5.2

### Development R version
* Ubuntu 14.04     (travis-ci), R 3.6.0 (2018-10-02 r75386)
* Ubuntu 18.04         (local), R 3.6.0 (2018-10-01 r75383)
* Windows           (appveyor), R 3.6.0 (2018-10-01 r75383)
* Windows        (win-builder), R 3.6.0 (2018-12-28 r75917)

## R CMD check results

There were no ERRORs or WARNINGs

There was 1 NOTE:

1. The URL given to install `fastshp` is in quotes in DESCRIPTION as it is part of a command.

## Downstream dependencies

There are currently no downstream dependencies of this package.
