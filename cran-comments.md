## Update

This update speeds up common searches (`inRadius()`, `of()`, `NLwith()`,
`NLset()`, `turtle()`), fixes bugs with multi-layer and character `worldArray`
objects, and drops the dependency on `quickPlot` (adding `dbscan`). Functions
that used to rely on `sp` are deprecated in favour of `sf`. See `NEWS.md`.

## Test environments

* Ubuntu 24.04 (local), R 4.6.1: `R CMD check --as-cran`
* GitHub Actions (org reusable workflows): see the badge on the repository
* win-builder (release, devel), macOS builder and R-hub: TO DO before submission

## R CMD check results

0 errors | 0 warnings | 0 notes (local; the only NOTE on the development
branch is about the development version number and the age of `Date`, which go
away when the version and date are set for the release).

## Reverse dependencies

`SpaDES.core` and `SpaDES.tools` list `NetLogoR` in Suggests only. Neither calls
`quickPlot::Plot()` on a `NetLogoR` object. `revdepcheck` TO DO before submission.
