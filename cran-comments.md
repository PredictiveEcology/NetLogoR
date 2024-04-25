## Updated release

This release is to put the package back on CRAN after being removed due to a dependency removal. There are other minor changes, including several to address CRAN requirements.
See `NEWS.md` for a full list of changes.

## Test environments

### Previous R versions
* Ubuntu 20.04                 (GitHub), R 4.2.3, 4.3.3
* Windows                      (GitHub), R 4.2.3, 4.3.3
* Windows                 (win-builder), R 4.3.3

### Current R versions
* macOS 12.6.3                 (GitHub), R 4.4.0
* macOS 13.3.1            (mac-builder), R 4.4.0
* macOS 14.4.1                  (local), R 4.4.0
* Ubuntu 20.04                 (GitHub), R 4.4.0
* Ubuntu 20.04                  (local), R 4.4.0
* Windows                      (GitHub), R 4.4.0
* Windows                       (local), R 4.4.0
* Windows                 (win-builder), R 4.4.0

### Development R version
* Ubuntu 20.04                 (GitHub), R-devel (2024-04-23 r86473)
* Ubuntu 20.04                  (local), R-devel (2024-04-24 r86484)
* Windows                      (GitHub), R-devel (2024-04-24 r86483 ucrt)
* Windows                 (win-builder), R-devel (2024-04-23 r86473 ucrt)

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
