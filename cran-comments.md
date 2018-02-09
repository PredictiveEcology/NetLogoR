## Resubmission

In this version we have fixed the use of a suggested package in tests, which was causing errors on CRAN.
We also fixed another bug (see `NEWS.md`).

## Test environments

### Previous R versions
* Ubuntu 14.04       (travis-ci), R 3.3.0
* Windows             (appveyor), R 3.3.0

* Ubuntu 14.04       (travis-ci), R 3.3.3
* Windows             (appveyor), R 3.3.3
* Windows 7              (local), R 3.3.3

### Current R versions
* macOS High Sierra  (travis-ci), R 3.4.3
* OSX Sierra 10.12.6 (travis-ci), R 3.4.3 
* Ubuntu 14.04       (travis-ci), R 3.4.3
* Windows             (appveyor), R 3.4.3
* Windows 7              (local), R 3.4.3
* Windows          (win-builder), R 3.4.3

### Development R version
* Ubuntu 14.04     (travis-ci), R 3.5.0 (2018-02-07 r74234)
* Windows           (appveyor), R 3.5.0 (2018-02-06 r74231)
* Windows              (local), R 3.5.0 (2018-02-07 r74234)
* Windows        (win-builder), R 3.5.0 (2018-02-07 r74234)

## R CMD check results

There were no ERRORs or WARNINGs

There were 3 NOTES:

    1. Some words were flagged as possibly mispelled, but these are false positives.
     
            Possibly mis-spelled words in DESCRIPTION: 
              Predation (16:29)
              Railsback (15:67)
              Wilensky (4:16)

    2. `fastshp` is on Rforge. We have put the link to the correct Additional_repositories in DESCRIPTION.
       
    3. The URL given to install `fastshp` is in quotes in DESCRIPTION as it is part of a command.

## Downstream dependencies

There are currently no downstream dependencies of this package.
