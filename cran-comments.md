## Updated release

This release is to put the package back on CRAN after being removed due to a dependency removal. There are other minor changes, including several to address CRAN requirements.
See `NEWS.md` for a full list of changes.

## Test environments

### Previous R versions
* Ubuntu 20.04                 (GitHub), R 4.2.3
* Ubuntu 20.04                 (GitHub), R 4.2.3
* Windows                      (GitHub), R 4.2.3
* Windows                      (GitHub), R 4.2.3
* Windows                 (win-builder), R 4.2.3

### Current R versions
* macOS 12.6.3 Monterey        (GitHub), 4.3.2 (2023-10-31)
* Ubuntu 20.04                 (GitHub), 4.3.2 (2023-10-31)
* Ubuntu 20.04                  (local), 4.3.2 (2023-10-31)
* Windows                      (GitHub), 4.3.2 (2023-10-31 ucrt)
* Windows                       (local), 4.3.2 (2023-10-31 ucrt)
* Windows                 (win-builder), 4.3.2 (2023-10-31 ucrt)

### Development R version
* Ubuntu 20.04                 (GitHub), R-devel (2023-07-03 r84633)
* Ubuntu 20.04                 (local),  R-devel (2023-07-03 r84633)
* Windows                      (GitHub), R-devel (2023-12-20 r85711 ucrt)
* Windows                 (win-builder), R-devel (2023-12-20 r85711 ucrt)
* Windows                       (local), R-devel (2023-12-20 r85711 ucrt)

## R CMD check results

There were no ERRORs or WARNINGs.

There was 2 NOTEs:

1. Package suggested but not available for checking: 'fastshp'. This is available at an "Additional_repositories" and the instructions are given:
Suggests or Enhances not in mainstream repositories:
  fastshp
Availability using Additional_repositories specification:
  fastshp   yes   https://predictiveecology.r-universe.dev/
2. The other is 3 possible spelling errors. These are all in the WORDLIST file and are correct.

## Downstream dependencies

There are currently no downstream dependencies of this package.
